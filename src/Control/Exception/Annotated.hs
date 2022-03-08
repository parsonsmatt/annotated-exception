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
-- While this library can be used directly, it is probably better to use it
module Control.Exception.Annotated
    ( module Control.Exception.Annotated
    , Exception(..)
    , Safe.SomeException(..)
    , Safe.throw
    ) where

import Control.Exception.Safe (Exception, MonadCatch, SomeException(..))
import qualified Control.Exception.Safe as Safe
import Data.Typeable

import Data.Annotation

-- | The 'AnnotatedException' type wraps an @exception@ with
-- a @['Annotation']@. This can provide a sort of a manual stack trace with
-- programmer provided data.
data AnnotatedException exception
    = AnnotatedException
    { annotations :: [Annotation]
    , exception   :: exception
    }
    deriving (Eq, Show)

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
new :: e -> AnnotatedException e
new = AnnotatedException []

-- | Append the @['Annotation']@ to the 'AnnotatedException'.
annotate :: [Annotation] -> AnnotatedException e -> AnnotatedException e
annotate ann (AnnotatedException anns e) = AnnotatedException (ann ++ anns) e

-- | Call 'toException' on the underlying 'Exception'.
hide :: Exception e => AnnotatedException e -> AnnotatedException SomeException
hide (AnnotatedException anns e) = AnnotatedException anns (Safe.toException e)

-- | Call 'fromException' on the underlying 'Exception', attaching the
-- annotations to the result.
check :: Exception e => AnnotatedException SomeException -> Maybe (AnnotatedException e)
check (AnnotatedException anns e) = AnnotatedException anns <$> Safe.fromException e

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
catch :: (Exception e, MonadCatch m) => m a -> (e -> m a) -> m a
catch action handler =
    Safe.catches
        action
        [ Safe.Handler handler
        , Safe.Handler $ \(AnnotatedException anns e) ->
            checkpointMany anns $ handler e
        ]

-- | Like 'catch', but always returns a 'AnnotatedException'.
try :: (Exception e, MonadCatch m) => m a -> m (Either (AnnotatedException e) a)
try action =
    (Right <$> action) `catch` (pure . Left)

-- | Concatenate two lists of annotations.
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
checkpoint :: MonadCatch m => Annotation -> m a -> m a
checkpoint ann = checkpointMany [ann]

-- | Add the list of 'Annotations' to any exception thrown in the following
-- action.
checkpointMany :: (MonadCatch m) => [Annotation] -> m a -> m a
checkpointMany ann action =
    action `Safe.catch` \(exn :: SomeException) ->
        Safe.throw . annotate ann $ case Safe.fromException exn of
            Just (e' :: AnnotatedException SomeException) ->
                e'
            Nothing -> do
                new exn
