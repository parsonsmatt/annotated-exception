{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

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
    , new
    , throwWithCallStack
    -- * Annotating Exceptions
    , checkpoint
    , checkpointMany
    , checkpointCallStack
    , checkpointCallStackWith
    -- * Handling Exceptions
    , catch
    , tryAnnotated
    -- * Manipulating Annotated Exceptions
    , check
    , hide
    , annotatedExceptionCallStack

    -- * Re-exports from "Data.Annotation"
    , Annotation(..)
    , CallStackAnnotation(..)
    -- * Re-exports from "Control.Exception.Safe"
    , Exception(..)
    , Safe.SomeException(..)
    , Safe.throw
    , Safe.try
    ) where

import Control.Exception.Safe
       (Exception, MonadCatch, MonadThrow, SomeException(..))
import qualified Control.Exception.Safe as Safe
import Data.Annotation
import Data.Typeable
import GHC.Stack
import Data.Maybe

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
    deriving (Eq, Show, Functor, Foldable, Traversable)

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
-- Likewise, if a @'AnnotatedException' ('AnnotatedException' e)@ is thrown
-- somehow, then the 'fromException' will flatten it and combine their
-- contexts.
--
-- For the most up to date details, see the test suite.
--
-- @since 0.1.0.0
instance (Exception exception) => Exception (AnnotatedException exception) where
    toException loc = SomeException $ hide loc
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
            pure $ new e
        | Just x <- flatten <$> Safe.fromException exn
        =
            pure x
        | otherwise
        =
            Nothing

-- | Attach an empty @['Annotation']@ to an exception.
--
-- @since 0.1.0.0
new :: e -> AnnotatedException e
new = pure

-- | Append the @['Annotation']@ to the 'AnnotatedException'.
--
-- @since 0.1.0.0
annotate :: [Annotation] -> AnnotatedException e -> AnnotatedException e
annotate ann (AnnotatedException anns e) = AnnotatedException (ann ++ anns) e

-- | Call 'toException' on the underlying 'Exception'.
--
-- @since 0.1.0.0
hide :: Exception e => AnnotatedException e -> AnnotatedException SomeException
hide = fmap Safe.toException

-- | Call 'fromException' on the underlying 'Exception', attaching the
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
-- We can throw an exception and catch it with location.
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
catch :: (Exception e, MonadCatch m) => m a -> (e -> m a) -> m a
catch action handler =
    Safe.catches
        action
        [ Safe.Handler handler
        , Safe.Handler $ \(AnnotatedException anns e) ->
            checkpointMany anns $ handler e
        ]

-- | Like 'catch', but always returns a 'AnnotatedException'.
--
-- @since 0.1.0.0
tryAnnotated :: (Exception e, MonadCatch m) => m a -> m (Either (AnnotatedException e) a)
tryAnnotated action =
    (Right <$> action) `catch` (pure . Left)

-- | Attaches the 'CallStack' to the 'AnnotatedException' that is thrown.
--
-- The 'CallStack' will *not* be present as a 'CallStack' - it will be
-- a 'CallStackAnnotation'.
--
-- @since 0.1.0.0
throwWithCallStack
    :: (HasCallStack, MonadThrow m, Exception e)
    => e -> m a
throwWithCallStack e =
    Safe.throw (AnnotatedException [callStackAnnotation] e)

-- | Concatenate two lists of annotations.
--
-- @since 0.1.0.0
flatten :: AnnotatedException (AnnotatedException e)  -> AnnotatedException e
flatten (AnnotatedException a (AnnotatedException b c)) = AnnotatedException (a ++ b) c

-- | Add a single 'Annotation' to any exceptions thrown in the following
-- action.
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
checkpoint :: MonadCatch m => Annotation -> m a -> m a
checkpoint ann = checkpointMany [ann]

-- | Add the current 'CallStack' to the checkpoint. This function searches any
-- thrown exception for a pre-existing 'CallStack' and will not overwrite or
-- replace the 'CallStack' if one is already present.
--
-- Primarily useful when you're wrapping a third party library.
--
-- @since 0.1.0.0
checkpointCallStackWith
    :: (MonadCatch m, HasCallStack)
    => [Annotation]
    -> m a
    -> m a
checkpointCallStackWith ann action =
    action `Safe.catch` \(exn :: SomeException) ->
        Safe.throw
            . annotate ann
            . addCallStackToException callStack
            $ case Safe.fromException exn of
                Just (e' :: AnnotatedException SomeException) ->
                    case annotatedExceptionCallStack e' of
                        Nothing ->
                            annotate ann e'
                        Just _preexistingCallstack ->
                            e'
                Nothing -> do
                    annotate ann $ new exn

-- | Add the current 'CallStack' to the checkpoint. This function searches any
-- thrown exception for a pre-existing 'CallStack' and will not overwrite or
-- replace the 'CallStack' if one is already present.
--
-- Primarily useful when you're wrapping a third party library.
--
-- @since 0.1.0.0
checkpointCallStack
    :: (MonadCatch m, HasCallStack)
    => m a
    -> m a
checkpointCallStack =
    checkpointCallStackWith []

-- | Add the list of 'Annotations' to any exception thrown in the following
-- action.
--
-- @since 0.1.0.0
checkpointMany :: (MonadCatch m) => [Annotation] -> m a -> m a
checkpointMany ann action =
    action `Safe.catch` \(exn :: SomeException) ->
        Safe.throw . annotate ann $ case Safe.fromException exn of
            Just (e' :: AnnotatedException SomeException) ->
                e'
            Nothing -> do
                new exn

-- | Retrieves the 'CallStack' from an 'AnnotatedException' if one is present.
--
-- @since 0.1.0.0
annotatedExceptionCallStack :: AnnotatedException exception -> Maybe CallStack
annotatedExceptionCallStack exn =
    let (stacks, _rest) = callStackInAnnotations (annotations exn)
    in listToMaybe stacks

-- | Adds a 'CallStack' to the given 'AnnotatedException'. This function will
-- search through the existing annotations, and it will not add a second
-- 'CallStack' to the list.
--
-- @since 0.1.0.0
addCallStackToException
    :: CallStack
    -> AnnotatedException exception
    -> AnnotatedException exception
addCallStackToException cs exn =
    case annotatedExceptionCallStack exn of
        Nothing ->
            annotate [callStackToAnnotation cs] exn
        Just _ ->
            exn
