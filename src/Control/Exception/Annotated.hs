{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- | This module defines an exception wrapper 'AnnotatedException' that
-- carries a list of 'Annotation's, along with some helper methods for
-- throwing and catching that can make the annotations transparent.
--
-- While this library can be used directly, it is recommended that you
-- define your own types and functions specific to your domain. As an
-- example, 'checkpoint' is useful *only* for providing exception
-- annotation information. However, you probably want to use 'checkpoint'
-- in concert with other context adding features, like logging.
--
-- Likewise, the 'Annotation' type defined in "Data.Annotation" is
-- essentially a wrapper for a dynamically typed value. So you probably
-- want to define your own 'checkpoint' that uses a custom type that you
-- want to enforce throughout your application.
module Control.Exception.Annotated
    ( -- * The Main Type
      AnnotatedException(..)
    , exceptionWithCallStack
    , throw
    , throwWithCallStack
    -- * Annotating Exceptions
    , checkpoint
    , checkpointMany
    , checkpointCallStack
    , checkpointCallStackWith
    -- * Handling Exceptions
    , catch
    , catches
    , tryAnnotated
    , try

    -- * Manipulating Annotated Exceptions
    , check
    , hide
    , annotatedExceptionCallStack
    , addCallStackToException

    -- * Re-exports from "Data.Annotation"
    , Annotation(..)
    , CallStackAnnotation(..)
    -- * Re-exports from "Control.Exception.Safe"
    , Exception(..)
    , Safe.SomeException(..)
    , Handler (..)
    ) where

import Control.Exception.Safe
       (Exception, Handler(..), MonadCatch, MonadThrow, SomeException(..))
import qualified Control.Exception.Safe as Safe
import Data.Annotation
import Data.Maybe
import qualified Data.Set as Set
import Data.Typeable
import GHC.Stack
import Control.Applicative ((<|>))

-- | The 'AnnotatedException' type wraps an @exception@ with
-- a @['Annotation']@. This can provide a sort of a manual stack trace with
-- programmer provided data.
--
-- @since 0.1.0.0
data AnnotatedException exception
    = AnnotatedException
    { annotations :: [Annotation]
    , exception   :: exception
    }
    deriving (Show, Functor, Foldable, Traversable)

instance Applicative AnnotatedException where
    pure =
        AnnotatedException []

    AnnotatedException anns0 f <*> AnnotatedException anns1 a =
        AnnotatedException (anns0 <> anns1) (f a)

-- | This instance of 'Exception' is a bit interesting. It tries to do as
-- much hiding and packing and flattening as possible to ensure that even
-- exception handling machinery outside of this package can still
-- intelligently handle it.
--
-- Any 'Exception' can be caught as a 'AnnotatedException' with
-- an empty context, so catching a @'AnnotatedException' e@ will also catch
-- a regular @e@ and give it an empty set of annotations.
--
-- For the most up to date details, see the test suite.
--
-- @since 0.1.0.0
instance (Exception exception) => Exception (AnnotatedException exception) where
    toException loc =
        tryFlatten $ SomeException $ hide loc

    fromException (SomeException exn)
        | Just x <- cast exn
        =
            pure x
        | Just (AnnotatedException ann (e :: SomeException)) <- cast exn
        , Just a <- Safe.fromException e
        =
            pure $ AnnotatedException ann a
    fromException exn
        | Just (e :: exception) <- Safe.fromException exn
        =
            pure $ pure e
        | otherwise
        =
            Nothing

-- | Annotate the underlying exception with a 'CallStack'.
--
-- @since 0.2.0.0
exceptionWithCallStack :: (Exception e, HasCallStack) => e -> AnnotatedException e
exceptionWithCallStack =
    AnnotatedException [callStackAnnotation]

-- | Append the @['Annotation']@ to the 'AnnotatedException'.
--
-- 'CallStack' is a special case - if a 'CallStack' is present in both the
-- 'AnnotatedException' and the @['Annotation']@, then this will append the
-- 'CallStack's in the new list and concatenate them all together.
--
-- @since 0.1.0.0
annotate :: [Annotation] -> AnnotatedException e -> AnnotatedException e
annotate newAnnotations (AnnotatedException oldAnnotations e) =
    let
        (callStacks, other) =
            tryAnnotations (newAnnotations <> oldAnnotations)
    in
        foldr addCallStackToException (AnnotatedException other e) callStacks

-- | Call 'Safe.toException' on the underlying 'Exception'.
--
-- @since 0.1.0.0
hide :: Exception e => AnnotatedException e -> AnnotatedException SomeException
hide = fmap Safe.toException

-- | Call 'Safe.fromException' on the underlying 'Exception', attaching the
-- annotations to the result.
--
-- @since 0.1.0.0
check :: Exception e => AnnotatedException SomeException -> Maybe (AnnotatedException e)
check = traverse Safe.fromException

-- | Catch an exception. This works just like 'Safe.catch', but it also
-- will attempt to catch @'AnnotatedException' e@. The annotations will be
-- preserved in the handler, so rethrowing exceptions will retain the
-- context.
--
-- Let's consider a few examples, that share this import and exception
-- type.
--
-- > import qualified Control.Exception.Safe as Safe
-- > import Control.Exception.Annotated
-- >
-- > data TestException deriving (Show, Exception)
--
-- We can throw an exception and catch it as usual.
--
-- > throw TestException `catch` \TestException ->
-- >     putStrLn "ok!"
--
-- We can throw an exception and catch it with annotations.
--
-- > throw TestException `catch` \(AnnotatedException anns TestException) ->
-- >     putStrLn "ok!"
--
--
-- We can throw an exception and catch it as a @'AnnotatedException'
-- 'SomeException'@.
--
-- > throw TestException `catch` \(AnnotatedException anns (e :: SomeException) ->
-- >     putStrLn "ok!"
--
-- @since 0.1.0.0
catch :: (HasCallStack, Exception e, MonadCatch m) => m a -> (e -> m a) -> m a
catch action handler =
    withFrozenCallStack catches action [Handler handler]

-- | Like 'Safe.catches', but this function enhance the provided 'Handler's
-- to "see through" any 'AnnotatedException's.
--
-- @since 0.1.2.0
catches :: (MonadCatch m, HasCallStack) => m a -> [Handler m a] -> m a
catches action handlers =
    Safe.catches action (withFrozenCallStack mkAnnotatedHandlers handlers)

-- | Extends each 'Handler' in the list with a variant that sees through
-- the 'AnnotatedException' and re-annotates any rethrown exceptions.
--
-- @since 0.1.1.0
mkAnnotatedHandlers :: (HasCallStack, MonadCatch m) => [Handler m a] -> [Handler m a]
mkAnnotatedHandlers xs =
    xs >>= \(Handler hndlr) ->
        [ Handler $ \e ->
            checkpointCallStack $ hndlr e
        , Handler $ \(AnnotatedException anns e) ->
            checkpointMany anns $ hndlr e
        ]

-- | Like 'catch', but always returns a 'AnnotatedException'.
--
-- @since 0.1.0.0
tryAnnotated :: (Exception e, MonadCatch m) => m a -> m (Either (AnnotatedException e) a)
tryAnnotated action =
    (Right <$> action) `catch` (pure . Left)

-- | Like 'Safe.try', but can also handle an 'AnnotatedException' or the
-- underlying value. Useful when you want to 'try' to catch a type of
-- exception, but you may not care about the 'Annotation's that it may or
-- may not have.
--
-- Example:
--
-- > Left exn <- try $ throw (AnnotatedException [] TestException)
-- > exn == TestException
--
-- > Left exn <- try $ throw TestException
-- > exn == AnnotatedException [] TestException
--
-- @since 0.1.0.1
try :: (Exception e, MonadCatch m) => m a -> m (Either e a)
try action =
    (Right <$> action)
      `catch`
          (\exn -> pure $ Left exn)

-- | Throws an 'Exception' and annotates it with the current 'CallStack'.
--
-- An alias for 'throwWithCallStack'.
--
-- @since 0.2.0.0
throw :: (HasCallStack, MonadThrow m, Exception e) => e -> m a
throw = withFrozenCallStack throwWithCallStack

-- | Attaches the 'CallStack' to the 'AnnotatedException' that is thrown.
--
-- @since 0.1.0.0
throwWithCallStack
    :: (HasCallStack, MonadThrow m, Exception e)
    => e -> m a
throwWithCallStack e =
    withFrozenCallStack $
        Safe.throw (AnnotatedException [callStackAnnotation] e)

-- | Concatenate two lists of annotations.
--
-- @since 0.1.0.0
flatten :: AnnotatedException (AnnotatedException e)  -> AnnotatedException e
flatten (AnnotatedException a (AnnotatedException b c)) = AnnotatedException (go Nothing a b) c
  where
    go :: Maybe CallStack -> [Annotation] -> [Annotation] -> [Annotation]
    go mcallstack [] bs =
        case mcallstack of
            Just cs ->
                addCallStackToAnnotations cs bs
            Nothing ->
                bs
    go mcallstack (a : as) bs =
        case castAnnotation a of
            Just cs ->
                let newAcc = fmap (mergeCallStack cs) mcallstack <|> Just cs
                 in go newAcc as bs
            Nothing ->
                a : go mcallstack as bs

tryFlatten :: SomeException -> SomeException
tryFlatten exn =
    case Safe.fromException exn of
        Just (a :: AnnotatedException (AnnotatedException SomeException)) ->
            SomeException $ flatten a
        Nothing ->
            exn

-- | Add a single 'Annotation' to any exceptions thrown in the following
-- action. The 'CallStack' present on any 'AnnotatedException' will also be
-- updated to include this location.
--
-- Example:
--
-- > main = do
-- >     checkpoint "Foo" $ do
-- >         print =<< readFile "I don't exist.markdown"
--
-- The exception thrown due to a missing file will now have an 'Annotation'
-- @"Foo"@.
--
-- @since 0.1.0.0
checkpoint :: (HasCallStack, MonadCatch m) => Annotation -> m a -> m a
checkpoint ann = withFrozenCallStack (checkpointMany [ann])

-- | Add the current 'CallStack' to the checkpoint, along with the given
-- annotations. This function merges 'CallStack's together, attempting to
-- preserve the call site ordering as GHC does it.
--
-- As of 0.2.0.0, an alias for 'checkpointMany'.
--
-- @since 0.1.0.0
checkpointCallStackWith
    :: (MonadCatch m, HasCallStack)
    => [Annotation]
    -> m a
    -> m a
checkpointCallStackWith anns =
    withFrozenCallStack (checkpointMany anns)

{-# DEPRECATED checkpointCallStackWith "As of 0.2.0.0 this is exactly equivalent to `checkpointMany`." #-}

-- | Adds only the current 'CallStack' to the checkpoint. This function searches
-- any thrown exception for a pre-existing 'CallStack' and will merge the given
-- pre-existing 'CallStack' with the one on this function, in an attempt to
-- preserve the actual call history.
--
-- @since 0.1.0.0
checkpointCallStack
    :: (MonadCatch m, HasCallStack)
    => m a
    -> m a
checkpointCallStack =
    withFrozenCallStack (checkpoint (Annotation callStack))

-- | Add the list of 'Annotation' to any exception thrown in the following
-- action.
--
-- @since 0.1.0.0
checkpointMany :: (MonadCatch m, HasCallStack) => [Annotation] -> m a -> m a
checkpointMany anns action =
    action `Safe.catch` \(exn :: SomeException) ->
        Safe.throw
            . addCallStackToException callStack
            . annotate anns
            $ case Safe.fromException exn of
                Just (e' :: AnnotatedException SomeException) ->
                    e'
                Nothing -> do
                    pure exn

-- | Retrieves the 'CallStack' from an 'AnnotatedException' if one is present.
--
-- The library maintains an internal check that a single 'CallStack' is present
-- in the list, so this only returns the first one found. If you have added
-- a 'CallStack' directly to the @['Annotation']@, then this will likely break.
--
-- @since 0.1.0.0
annotatedExceptionCallStack :: AnnotatedException exception -> Maybe CallStack
annotatedExceptionCallStack exn =
    let (stacks, _rest) = tryAnnotations (annotations exn)
    in listToMaybe stacks

-- | Adds a 'CallStack' to the given 'AnnotatedException'. This function will
-- search through the existing annotations, and it will not add a second
-- 'CallStack' to the list. Instead, it will append the contents of the given
-- 'CallStack' to the existing one.
--
-- This mirrors the behavior of the way 'HasCallStack' actually works.
--
-- @since 0.1.0.0
addCallStackToException
    :: CallStack
    -> AnnotatedException exception
    -> AnnotatedException exception
addCallStackToException cs (AnnotatedException anns e) =
    AnnotatedException (addCallStackToAnnotations cs anns) e

addCallStackToAnnotations :: CallStack -> [Annotation] -> [Annotation]
addCallStackToAnnotations cs anns = go anns
  where
    -- not a huge fan of the direct recursion, but it seems easier than trying
    -- to finagle a `foldr` or something
    go [] =
        [Annotation cs]
    go (ann : anns) =
        case castAnnotation ann of
            Just preexistingCallStack ->
                Annotation (mergeCallStack preexistingCallStack cs) : anns
            Nothing ->
                ann : go anns

-- we want to merge callstack but not duplicate entries
mergeCallStack :: CallStack -> CallStack -> CallStack
mergeCallStack pre new =
        fromCallSiteList
        $ fmap (fmap fromSrcLocOrd)
        $ ordNub
        $ fmap (fmap toSrcLocOrd)
        $ getCallStack pre <> getCallStack new
  where
    toSrcLocOrd (SrcLoc a b c d e f g) =
        (a, b, c, d, e, f, g)
    fromSrcLocOrd (a, b, c, d, e, f, g) =
        SrcLoc a b c d e f g

-- | Remove duplicates but keep elements in order.
--   O(n * log n)
-- Vendored from GHC
ordNub :: Ord a => [a] -> [a]
ordNub = go Set.empty
  where
    go _ [] = []
    go s (x:xs)
      | Set.member x s = go s xs
      | otherwise = x : go (Set.insert x s) xs
