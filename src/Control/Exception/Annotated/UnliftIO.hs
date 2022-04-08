{-# language ExplicitForAll #-}

-- | This module presents the same interface as
-- "Control.Exception.Annotated", but uses 'MonadUnliftIO' instead of
-- 'MonadCatch' or 'MonadThrow'.
--
-- @since 0.1.2.0
module Control.Exception.Annotated.UnliftIO
    ( -- * The Main Type
      AnnotatedException(..)
    , exceptionWithCallStack
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
    , throw
    , Handler (..)
    , MonadIO(..)
    , MonadUnliftIO(..)
    ) where

import Control.Exception.Annotated hiding
       ( catch
       , catches
       , checkpoint
       , checkpointCallStackWith
       , checkpointMany
       , throw
       , throwWithCallStack
       , try
       , tryAnnotated
       )
import qualified Control.Exception.Annotated as Catch
import qualified Control.Exception.Safe as Safe
import Control.Monad.IO.Unlift
import GHC.Stack

-- | Like 'Catch.throwWithCallStack', but uses 'MonadIO' instead of
-- 'MonadThrow'.
--
-- @since 0.1.2.0
throwWithCallStack
    :: forall e m a. (MonadIO m, Exception e, HasCallStack)
    => e -> m a
throwWithCallStack = liftIO . withFrozenCallStack . Catch.throwWithCallStack

-- | Like 'Catch.throw', but uses 'MonadIO' instead of 'MonadThrow'.
--
-- @since 0.1.2.0
throw :: forall e m a. (MonadIO m, Exception e, HasCallStack) => e -> m a
throw = liftIO . withFrozenCallStack Catch.throw

-- | Like 'Catch.checkpoint', but uses 'MonadUnliftIO' instead of 'MonadCatch'.
--
-- @since 0.1.2.0
checkpoint :: forall m a. (MonadUnliftIO m, HasCallStack) => Annotation -> m a -> m a
checkpoint ann action = withRunInIO $ \runInIO ->
    liftIO $ withFrozenCallStack (Catch.checkpoint ann) (runInIO action)

-- | Like 'Catch.checkpointMany', but uses 'MonadUnliftIO' instead of
-- 'MonadCatch'.
--
-- @since 0.1.2.0
checkpointMany :: forall m a. (MonadUnliftIO m) => [Annotation] -> m a -> m a
checkpointMany anns action =
    withRunInIO $ \runInIO ->
        liftIO $ Catch.checkpointMany anns (runInIO action)

-- | Like 'Catch.checkpointCallStackWith', but uses 'MonadUnliftIO' instead of
-- 'MonadCatch'.
--
-- @since 0.1.2.0
checkpointCallStackWith
    :: forall m a. (MonadUnliftIO m, HasCallStack)
    => [Annotation] -> m a -> m a
checkpointCallStackWith anns action =
    withRunInIO $ \runInIO ->
        liftIO $ Catch.checkpointCallStackWith anns (runInIO action)

-- | Like 'Catch.catch', but uses 'MonadUnliftIO' instead of 'MonadCatch'.
--
-- @since 0.1.2.0
catch
    :: forall e m a. (MonadUnliftIO m, Exception e)
    => m a
    -> (e -> m a)
    -> m a
catch action handler =
    withRunInIO $ \runInIO ->
        liftIO $ Catch.catch (runInIO action) (\e -> runInIO $ handler e)

-- | Like 'Catch.tryAnnotated' but uses 'MonadUnliftIO' instead of 'MonadCatch'.
--
-- @since 0.1.2.0
tryAnnotated
    :: forall e m a. (MonadUnliftIO m, Exception e)
    => m a
    -> m (Either (AnnotatedException e) a)
tryAnnotated action =
    withRunInIO $ \runInIO ->
        liftIO $ Catch.tryAnnotated (runInIO action)

-- | Like 'Catch.try' but uses 'MonadUnliftIO' instead of 'MonadCatch'.
--
-- @since 0.1.2.0
try
    :: forall e m a. (MonadUnliftIO m, Exception e)
    => m a
    -> m (Either e a)
try action =
    withRunInIO $ \runInIO ->
        liftIO $ Catch.try (runInIO action)

-- | Like 'Catch.catches', bt uses 'MonadUnliftIO' instead of 'MonadCatch'.
--
-- @since 0.1.2.0
catches
    :: forall m a. MonadUnliftIO m
    => m a
    -> [Handler m a]
    -> m a
catches action handlers =
    withRunInIO $ \runInIO -> do
        let f (Handler k) = Handler (\e -> runInIO (k e))
        liftIO $ catches (runInIO action) (map f handlers)
  where
