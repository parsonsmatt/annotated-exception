{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# options_ghc -fno-warn-orphans -fno-warn-type-defaults #-}

module Control.Exception.AnnotatedSpec where

import Test.Hspec

import Control.Exception.Annotated
import qualified Control.Exception.Safe as Safe

data TextException = TestException
    deriving (Eq, Show, Exception)

instance Eq SomeException where
    e0 == e1 = show e0 == show e1

pass :: Expectation
pass = pure ()

spec :: Spec
spec = do
    describe "AnnotatedException can fromException a" $ do
        it "different type" $ do
            fromException (toException TestException)
                `shouldBe`
                    Just (new TestException)

        it "SomeException" $ do
            fromException (SomeException TestException)
                `shouldBe`
                    Just (new (SomeException TestException))

        it "nested AnnotatedException" $ do
            fromException (toException (new (new TestException)))
                `shouldBe`
                    Just (new TestException)

    describe "throw" $ do
        it "wraps exceptions" $ do
            throw TestException
                `shouldThrow`
                    \(AnnotatedException _ TestException) ->
                        True

    describe "catch" $ do
        it "catches located exceptions" $ do
            Safe.throw TestException
                `catch`
                    \(AnnotatedException [] TestException) ->
                        pass

        it "catches regular exceptions" $ do
            Safe.throw TestException
                `catch`
                    \TestException ->
                        pass

        it "catches SomeException" $ do
            throw TestException
                `catch`
                    \(SomeException _) ->
                        pass

        it "catches located SomeExceptions" $ do
            throw TestException
                `catch`
                    \(AnnotatedException _ (_ :: SomeException)) ->
                        pass

    describe "try" $ do
        it "always adds a location" $ do
            Left exn <- try (throw TestException)
            exn `shouldBe` AnnotatedException [] TestException

        it "does not nest locations" $ do
            Left exn <- try $ throw $ new $ new $ new TestException
            exn `shouldBe` new TestException

    describe "Safe.try" $ do
        it "can catch a located exception" $ do
            Left exn <- Safe.try (Safe.throw TestException)
            exn `shouldBe` new TestException

        it "does not catch a AnnotatedException" $ do
            let action = do
                    Left exn <- Safe.try (Safe.throw $ new TestException)
                    exn `shouldBe` TestException
            action `shouldThrow` (== new TestException)

    describe "checkpoint" $ do
        it "adds annotations" $ do
            Left exn <- try (checkpoint "Here" (throw TestException))
            exn `shouldBe` AnnotatedException ["Here"] TestException

        it "adds two annotations" $ do
            Left exn <- try $ do
                checkpoint "Here" $ do
                    checkpoint "There" $ do
                        throw TestException
            exn `shouldBe` AnnotatedException ["Here", "There"] TestException

        it "adds three annotations" $ do
            Left exn <- try $
                checkpoint "Here" $
                checkpoint "There" $
                checkpoint "Everywhere" $
                throw TestException
            exn `shouldBe` AnnotatedException ["Here", "There", "Everywhere"] TestException

        it "caught exceptions are propagated" $ do
            eresp <- try $
                checkpoint "Here" $
                flip catch (\TestException -> pure "Hello") $
                checkpoint "There" $
                checkpoint "Everywhere" $
                throw TestException
            case eresp of
                Left (AnnotatedException _ TestException) ->
                    expectationFailure "Should not be an exception"
                Right resp ->
                    resp `shouldBe` "Hello"

        it "works with error calls" $ do
            eresp <- checkpoint "Yes" (error "Oh no") `catch`
                \(SomeException _) -> pure "bar"
            eresp `shouldBe` "bar"

        it "works with non-handled exceptions" $ do
            Left exn <- try $
                checkpoint "Lmao" $
                Safe.throw TestException
            exn `shouldBe` AnnotatedException ["Lmao"] TestException

        it "supports rethrowing" $ do
            Left exn <- try $
                checkpoint "A" $
                flip catch (\TestException -> throw TestException) $
                checkpoint "B" $
                throw TestException
            exn `shouldBe` AnnotatedException ["A", "B"] TestException
