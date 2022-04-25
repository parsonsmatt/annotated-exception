{-# language RecordWildCards, StrictData, RankNTypes #-}

module Control.Exception.Annotated.UnliftIOSpec where

import Test.Hspec

import Control.Exception.Annotated.UnliftIO
import qualified Control.Exception.Safe as Safe
import Data.Annotation
import GHC.Stack

import Data.AnnotationSpec ()
import Control.Exception.AnnotatedSpec (TestException(..))
import Data.Maybe

asIO :: IO a -> IO a
asIO = id

spec :: Spec
spec = do
    describe "catch" $ do
        describe "the right exception" $ do
            it "works" $ asIO $ do
                throw TestException
                    `catch` \(AnnotatedException _ TestException) ->
                        pure ()

        describe "the wrong exception" $ do
            it "works" $ asIO $ do
                let
                    action =
                        throw (userError "oh no")
                            `catch` \(AnnotatedException _ TestException) ->
                                expectationFailure "Should not catch"
                action `shouldThrow` \(AnnotatedException _ e) ->
                    e == userError "oh no"

    describe "catches" $ do
        describe "the right exception" $ do
            it "works" $ asIO $ do
                throw TestException
                    `catches`
                        [ Handler $ \TestException ->
                            pure ()
                        ]
