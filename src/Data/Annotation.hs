{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Annotation
    ( module Data.Annotation
    , module Data.Proxy
    ) where

import           Data.Dynamic
import           Data.Proxy
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Data.String
import qualified Data.Text     as Text
import           Data.Typeable

type AnnC a = (Typeable a, Eq a, Show a)

data Annotation where
    Annotation
        :: AnnC a
        => a
        -> Annotation

instance Eq Annotation where
    Annotation (a :: a) == Annotation (b :: b) =
        case eqT @a @b of
            Just Refl ->
                a == b
            Nothing ->
                False

deriving instance Show Annotation

instance IsString Annotation where
    fromString = Annotation . Text.pack

toAnnotation :: (AnnC a) => a -> Annotation
toAnnotation = Annotation

tryAnnotation
    :: forall a. (Typeable a)
    => Proxy a
    -> Annotation
    -> Either a Annotation
tryAnnotation prxy a@(Annotation (val :: x)) =
    case eqT @a @x of
        Just Refl ->
            Left val
        Nothing ->
            Right a

tryAnnotations
    :: forall a. (Typeable a)
    => Proxy a
    -> [Annotation]
    -> [Either a Annotation]
tryAnnotations p = map (tryAnnotation p)

annotationTypes
    :: [Annotation]
    -> Set TypeRep
annotationTypes = Set.fromList . map (\(Annotation a) -> typeOf a)
