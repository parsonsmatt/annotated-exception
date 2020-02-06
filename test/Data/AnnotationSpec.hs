{-# language TypeApplications, FlexibleContexts, DataKinds #-}

module Data.AnnotationSpec where

import Test.Hspec
import Data.Annotation

spec :: Spec
spec = do
    describe "Eq" $ do
        it "works for equal values" $ do
            toAnnotation "hello" == toAnnotation "hello"
        it "works for non-equal values of smae type" $ do
            toAnnotation "a" /= toAnnotation "b"
        it "works for values of different types" $ do
            toAnnotation (1 :: Int) /= toAnnotation "a"
    describe "Show" $ do
        it "works" $ do
            show (toAnnotation (3 :: Int))
                `shouldBe`
                    "Annotation 3"
