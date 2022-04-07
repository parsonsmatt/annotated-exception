{-# LANGUAGE DataKinds #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.AnnotationSpec where

import Data.Typeable
import Data.Annotation
import Test.Hspec

spec :: Spec
spec = do
    describe "Show" $ do
        it "works" $ do
            show (toAnnotation (3 :: Int))
                `shouldBe`
                    "3"

instance Eq Annotation where
    Annotation (a :: a) == Annotation (b :: b) =
        case eqT @a @b of
            Just Refl ->
                show a == show b
            Nothing ->
                False
