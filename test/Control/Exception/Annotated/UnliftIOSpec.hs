{-# language ScopedTypeVariables, RecordWildCards, StrictData, RankNTypes #-}

module Control.Exception.Annotated.UnliftIOSpec where

import Test.Hspec

import Control.Exception.Annotated.UnliftIO
import qualified Control.Exception.Safe as Safe
import Data.Annotation
import GHC.Stack

import Data.AnnotationSpec ()
import Control.Exception.AnnotatedSpec
    ( TestException(..)
    , callStackFunctionNamesShouldBe
    )
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
        it "includes a callstack location" $ do
            let
                action =
                    throw TestException
                        `catch`
                            \TestException ->
                                throw TestException
            action
                `Safe.catch`
                        \(e :: AnnotatedException TestException) -> do
                            annotations e
                                `callStackFunctionNamesShouldBe`
                                    [ "throw"
                                    , "throw"
                                    , "catch"
                                    ]
    describe "catches" $ do
        it "has a callstack entry" $ do
            let
                action =
                    throw TestException
                        `catches`
                            [ Handler $ \TestException ->
                                throw TestException
                            ]
            action
                `Safe.catch`
                        \(e :: AnnotatedException TestException) -> do
                            annotations e
                                `callStackFunctionNamesShouldBe`
                                    [ "throw"
                                    , "throw"
                                    , "catches"
                                    ]


        describe "the right exception" $ do
            it "works" $ asIO $ do
                throw TestException
                    `catches`
                        [ Handler $ \TestException ->
                            pure ()
                        ]
