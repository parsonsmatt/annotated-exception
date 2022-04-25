{-# LANGUAGE ExplicitForAll #-}

-- | This module presents the same interface as
-- "Control.Exception.Annotated", but uses 'MonadUnliftIO' instead of
-- 'Control.Monad.Catch.MonadCatch' or 'Control.Monad.Catch.MonadThrow'.
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

import Control.Exception.Annotated
       ( AnnotatedException(..)
       , Annotation(..)
       , CallStackAnnotation(..)
       , Exception(..)
       , Handler(..)
       , addCallStackToException
       , annotatedExceptionCallStack
       , check
       , exceptionWithCallStack
       , hide
       )
import qualified Control.Exception.Annotated as Catch
import qualified Control.Exception.Safe as Safe
import Control.Monad.IO.Unlift
import GHC.Stack

-- | Like 'Catch.throwWithCallStack', but uses 'MonadIO' instead of
-- 'Control.Monad.Catch.MonadThrow'.
--
-- @since 0.1.2.0
throwWithCallStack
    :: forall e m a. (MonadIO m, Exception e, HasCallStack)
    => e -> m a
throwWithCallStack = liftIO . withFrozenCallStack Catch.throwWithCallStack

-- | Like 'Catch.throw', but uses 'MonadIO' instead of 'Control.Monad.Catch.MonadThrow'.
--
-- @since 0.1.2.0
throw :: forall e m a. (MonadIO m, Exception e, HasCallStack) => e -> m a
throw = liftIO . withFrozenCallStack Catch.throw

-- | Like 'Catch.checkpoint', but uses 'MonadUnliftIO' instead of 'Control.Monad.Catch.MonadCatch'.
--
-- @since 0.1.2.0
checkpoint :: forall m a. (MonadUnliftIO m, HasCallStack) => Annotation -> m a -> m a
checkpoint ann action = withRunInIO $ \runInIO ->
    liftIO $ withFrozenCallStack (Catch.checkpoint ann) (runInIO action)

-- | Like 'Catch.checkpointCallStack', but uses 'MonadUnliftIO' instead of
-- 'Control.Monad.Catch.MonadCatch'.
--
-- @since 0.2.0.2
checkpointCallStack
    :: forall m a. (MonadUnliftIO m, HasCallStack)
    => m a
    -> m a
checkpointCallStack action =
    withRunInIO $ \runInIO ->
        withFrozenCallStack Catch.checkpointCallStack (runInIO action)

-- | Like 'Catch.checkpointMany', but uses 'MonadUnliftIO' instead of
-- 'Control.Monad.Catch.MonadCatch'.
--
-- @since 0.1.2.0
checkpointMany :: forall m a. (MonadUnliftIO m, HasCallStack) => [Annotation] -> m a -> m a
checkpointMany anns action =
    withRunInIO $ \runInIO ->
        liftIO $ withFrozenCallStack Catch.checkpointMany anns (runInIO action)

-- | Like 'Catch.checkpointCallStackWith', but uses 'MonadUnliftIO' instead of
-- 'Control.Monad.Catch.MonadCatch'.
--
-- Deprecated in 0.2.0.0 as it is now an alias for 'checkpointMany'.
--
-- @since 0.1.2.0
checkpointCallStackWith
    :: forall m a. (MonadUnliftIO m, HasCallStack)
    => [Annotation] -> m a -> m a
checkpointCallStackWith anns action =
    withRunInIO $ \runInIO ->
        liftIO $ withFrozenCallStack Catch.checkpointCallStackWith anns (runInIO action)

{-# DEPRECATED checkpointCallStackWith "As of annotated-exception-0.2.0.0, this is an alias for checkpointMany" #-}

-- | Like 'Catch.catch', but uses 'MonadUnliftIO' instead of 'Control.Monad.Catch.MonadCatch'.
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

-- | Like 'Catch.tryAnnotated' but uses 'MonadUnliftIO' instead of 'Control.Monad.Catch.MonadCatch'.
--
-- @since 0.1.2.0
tryAnnotated
    :: forall e m a. (MonadUnliftIO m, Exception e)
    => m a
    -> m (Either (AnnotatedException e) a)
tryAnnotated action =
    withRunInIO $ \runInIO ->
        liftIO $ Catch.tryAnnotated (runInIO action)

-- | Like 'Catch.try' but uses 'MonadUnliftIO' instead of 'Control.Monad.Catch.MonadCatch'.
--
-- @since 0.1.2.0
try
    :: forall e m a. (MonadUnliftIO m, Exception e)
    => m a
    -> m (Either e a)
try action =
    withRunInIO $ \runInIO ->
        liftIO $ Catch.try (runInIO action)

-- | Like 'Catch.catches', bt uses 'MonadUnliftIO' instead of 'Control.Monad.Catch.MonadCatch'.
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
        liftIO $ Catch.catches (runInIO action) (map f handlers)
