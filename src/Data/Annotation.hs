{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- | An 'Annotation' is attached to a 'LocatedException'. They're
-- essentially a dynamically typed value with a convenient 'IsString'
-- instance. I'd recommend using something like @Data.Aeson.Value@ or
-- possibly something more strongly typed.
module Data.Annotation
    ( module Data.Annotation
    , module Data.Proxy
    ) where

import GHC.Stack
import Data.Dynamic
import Data.Either
import Data.Maybe
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import qualified Data.Text as Text
import Data.Typeable

-- | The constraints that the value inside an 'Annotation' must have.
--
-- We want 'Typeable' so we can do 'cast' and potentially get more useful
-- information out of it.
--
-- @since 0.1.0.0
type AnnC a = (Typeable a, Eq a, Show a)

-- | An 'Annotation' is a wrapper around a value that includes a 'Typeable'
-- constraint so we can later unpack it. It is essentially a 'Dynamic, but
-- we also include 'Show' and 'Eq' so it's more useful.
--
-- @since 0.1.0.0
data Annotation where
    Annotation
        :: AnnC a
        => a
        -> Annotation

-- |
--
-- @since 0.1.0.0
instance Eq Annotation where
    Annotation (a :: a) == Annotation (b :: b) =
        case eqT @a @b of
            Just Refl ->
                a == b
            Nothing ->
                False

-- |
--
-- @since 0.1.0.0
instance Show Annotation where
    show (Annotation a) = show a

-- |
--
-- @since 0.1.0.0
instance IsString Annotation where
    fromString = Annotation . Text.pack

-- | Wrap a value in an 'Annotation'.
--
-- @since 0.1.0.0
toAnnotation :: (AnnC a) => a -> Annotation
toAnnotation = Annotation

-- | Attempt to 'cast' the underlying value out of an 'Annotation'.
--
-- @since 0.1.0.0
castAnnotation
    :: forall a. (Typeable a)
    => Annotation
    -> Maybe a
castAnnotation (Annotation ann) =
    cast ann

-- | Attempt to 'cast' the underlying value out of an 'Annotation'.
-- Returns the original 'Annotation' if the cast isn't right.
--
-- @since 0.1.0.0
tryAnnotation
    :: forall a. (Typeable a)
    => Annotation
    -> Either a Annotation
tryAnnotation a@(Annotation val) =
    case cast val of
        Just x ->
            Left x
        Nothing ->
            Right a

-- | Attempt to 'cast' list of 'Annotation' into the given type. Any
-- 'Annotation' that is not in that form is left untouched.
--
-- @since 0.1.0.0
tryAnnotations
    :: forall a. (Typeable a)
    => [Annotation]
    -> ([a], [Annotation])
tryAnnotations = partitionEithers . map tryAnnotation

-- | Returns the 'Set' of types that are in the given annotations.
--
-- @since 0.1.0.0
annotationTypes
    :: [Annotation]
    -> Set TypeRep
annotationTypes = Set.fromList . map (\(Annotation a) -> typeOf a)

-- | Map a function over the given 'Annotation'. If the types don't match
-- up, then the whole thing returns 'Nothing'.
--
-- @since 0.1.0.0
mapAnnotation
    :: ((AnnC a, AnnC b))
    => (a -> b)
    -> Annotation
    -> Maybe Annotation
mapAnnotation f (Annotation ann) =
    Annotation . f <$> cast ann

-- | Map a function over the 'Annotation', leaving it unchanged if the
-- types don't match.
--
-- @since 0.1.0.0
mapMaybeAnnotation
    :: (AnnC a, AnnC b)
    => (a -> b)
    -> Annotation
    -> Annotation
mapMaybeAnnotation f ann =
    fromMaybe ann (mapAnnotation f ann)

-- | A wrapper type for putting a 'CallStack' into an 'Annotation'. We need
-- this because 'CallStack' does not have an 'Eq' instance.
--
-- @since 0.1.0.0
newtype CallStackAnnotation = CallStackAnnotation
    { unCallStackAnnotation :: [(String, SrcLoc)]
    }
    deriving (Eq, Show)

-- | Grab an 'Annotation' corresponding to the 'CallStack' that is
-- currently in scope.
--
-- @since 0.1.0.0
callStackAnnotation :: HasCallStack => Annotation
callStackAnnotation = Annotation $ CallStackAnnotation $ getCallStack callStack

-- | Attempt to convert an 'Annotation' back into a 'CallStack'.
--
-- @since 0.1.0.0
callStackFromAnnotation :: CallStackAnnotation -> CallStack
callStackFromAnnotation ann =
    fromCallSiteList $ unCallStackAnnotation ann

-- | Extract the 'CallStack's from the @['Annotation']@. Any 'Annotation'
-- not corresponding to a 'CallStack' will be in the second element of the
-- tuple.
--
-- @since 0.1.0.0
callStackInAnnotations :: [Annotation] -> ([CallStack], [Annotation])
callStackInAnnotations anns =
    let (callStacks, rest) =
            tryAnnotations anns
    in
        (fmap callStackFromAnnotation callStacks, rest)
