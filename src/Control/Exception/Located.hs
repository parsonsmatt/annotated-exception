{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}

module Control.Exception.Located
    ( module Control.Exception.Located
    , Exception(..)
    , Safe.SomeException(..)
    ) where

import           Control.Exception.Safe (Exception, MonadCatch (..),
                                         MonadThrow (..), SomeException (..))
import qualified Control.Exception.Safe as Safe
import           Data.Text
import           Data.Typeable

-- | This is a placeholder type synonym while I work out the throwing and
-- catching bits.
type Annotation = Text

data LocatedException exception
    = LocatedException
    { annotations :: [Annotation]
    , exception :: exception
    }
    deriving (Eq, Show)

instance (Exception exception) => Exception (LocatedException exception)

new :: e -> LocatedException e
new = LocatedException []

annotate :: [Annotation] -> LocatedException e -> LocatedException e
annotate ann (LocatedException anns e) = LocatedException (ann ++ anns) e

hide :: Exception e => LocatedException e -> LocatedException SomeException
hide (LocatedException anns e) = LocatedException anns (Safe.toException e)

check :: Exception e => LocatedException SomeException -> Maybe (LocatedException e)
check (LocatedException anns e) = LocatedException anns <$> Safe.fromException e

-- | Throw an exception and give it an empty annotation.
throw :: (Exception e, MonadThrow m) => e -> m a
throw = Safe.throw . new

-- | Catch an exception. Also tries to catch a 'LocatedException' of the
-- same type.
catch :: (Exception e, MonadCatch m) => m a -> (e -> m a) -> m a
catch action handler =
    Safe.catches
        action
        [ Safe.Handler handler
        , Safe.Handler $ \x@(LocatedException acc exn) ->
            checkpointMany acc $ case Safe.fromException exn of
                Nothing -> do
                    case Safe.fromException exn of
                        Nothing ->
                            Safe.throw x
                        Just (LocatedException anns e) ->
                            checkpointMany anns $ handler e
                Just e -> do
                    handler e
        , Safe.Handler $ \(LocatedException anns e) ->
            checkpointMany anns $ handler e
        ]

try :: (Exception e, MonadCatch m) => m a -> m (Either (LocatedException e) a)
try action =
    Safe.catches (Right <$> action)
        [ Safe.Handler $ pure . Left
        , Safe.Handler $ \e -> pure (Left (new e))
        , Safe.Handler $ \e -> pure (Left (flatten e))
        , Safe.Handler $ \x@(LocatedException anns exn) -> do
            case Safe.fromException exn of
                Just e ->
                    pure (Left (LocatedException anns e))
                Nothing ->
                    case Safe.fromException exn of
                        Just e ->
                            pure (Left (flatten (LocatedException anns e)))
                        Nothing ->
                            Safe.throw x
        ]

flatten :: LocatedException (LocatedException e)  -> LocatedException e
flatten (LocatedException a (LocatedException b c)) = LocatedException (a ++ b) c

checkpoint :: MonadCatch m => Annotation -> m a -> m a
checkpoint ann = checkpointMany [ann]

checkpointMany :: (MonadCatch m) => [Annotation] -> m a -> m a
checkpointMany ann action =
    action `Safe.catch` \(exn :: SomeException) ->
        Safe.throw . annotate ann $ case Safe.fromException exn of
            Just e'@(LocatedException anns (e :: SomeException)) ->
                e'
            Nothing -> do
                new exn
