{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, OverloadedStrings #-}

module Control.Exception.LocatedSpec where

import           Test.Hspec

import qualified Control.Exception.Safe as Safe
import           Control.Exception.Located

data TextException = TestException
    deriving (Eq, Show, Exception)

pass :: Expectation
pass = pure ()

spec :: Spec
spec = do
    describe "throw" $ do
        it "wraps exceptions" $ do
            throw TestException
                `shouldThrow`
                    \(LocatedException _ TestException) ->
                        True

    describe "catch" $ do
        it "catches located exceptions" $ do
            throw TestException
                `catch`
                    \(LocatedException _ TestException) ->
                        pass

        it "catches regular exceptions" $ do
            throw TestException
                `catch`
                    \TestException ->
                        pass

        it "catches located SomeExceptions" $ do
            pendingWith "This does not actually work"
            throw TestException
                `catch`
                    \(LocatedException _ (e :: SomeException)) ->
                        pass

    describe "try" $ do
        it "traps exceptions" $ do
            Left exn <- try (throw TestException)
            exn `shouldBe` LocatedException [] TestException

    describe "checkpoint" $ do
        it "adds annotations" $ do
            Left exn <- try (checkpoint "Here" (throw TestException))
            exn `shouldBe` LocatedException ["Here"] TestException

        it "adds two annotations" $ do
            Left exn <- try $ do
                checkpoint "Here" $ do
                    checkpoint "There" $ do
                        throw TestException
            exn `shouldBe` LocatedException ["Here", "There"] TestException

        it "adds three annotations" $ do
            Left exn <- try $
                checkpoint "Here" $
                checkpoint "There" $
                checkpoint "Everywhere" $
                throw TestException
            exn `shouldBe` LocatedException ["Here", "There", "Everywhere"] TestException

        it "caught exceptions are propagated" $ do
            eresp <- try $
                checkpoint "Here" $
                flip catch (\TestException -> pure "Hello") $
                checkpoint "There" $
                checkpoint "Everywhere" $
                throw TestException
            case eresp of
                Left (LocatedException _ TestException) ->
                    error "Should not be an exception"
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
            exn `shouldBe` LocatedException ["Lmao"] TestException

        it "supports rethrowing" $ do
            Left exn <- try $
                checkpoint "A" $
                flip catch (\TestException -> throw TestException) $
                checkpoint "B" $
                throw TestException
            exn `shouldBe` LocatedException ["A", "B"] TestException
