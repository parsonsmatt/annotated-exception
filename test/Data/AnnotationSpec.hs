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
        it "includes type information" $ do
            show (Annotation @Int 3)
                `shouldBe`
                    "Annotation @Int 3"
        it "uses parens" $ do
            show (Annotation @(Maybe Int) (Just 3))
                `shouldBe`
                    "Annotation @(Maybe Int) (Just 3)"
        it "is cool with strings" $ do
            show (Annotation @String "Hello")
                `shouldBe`
                    "Annotation @[Char] \"Hello\""

instance Eq Annotation where
    Annotation (a :: a) == Annotation (b :: b) =
        case eqT @a @b of
            Just Refl ->
                show a == show b
            Nothing ->
                False
