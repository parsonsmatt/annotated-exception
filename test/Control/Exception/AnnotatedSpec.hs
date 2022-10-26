{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# options_ghc -fno-warn-orphans -fno-warn-type-defaults #-}

module Control.Exception.AnnotatedSpec where

import Test.Hspec

import qualified UnliftIO.Exception as UnliftIO
import Control.Concurrent.Async (ExceptionInLinkedThread(..), Async, asyncThreadId)
import qualified Control.Exception
import Control.Exception (SomeAsyncException(..))
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import qualified Control.Concurrent
import qualified UnliftIO.Concurrent
import UnliftIO.Concurrent (threadDelay)
import Control.Exception.Annotated
import qualified Control.Exception.Safe as Safe
import Data.Annotation
import GHC.Stack
import Data.Typeable
import Data.AnnotationSpec ()
import Data.Maybe
import qualified UnliftIO.Async as UnliftIO
import qualified Control.Concurrent.Async as Async
import Control.Monad

instance Eq CallStack where
    a == b = show a == show b

deriving stock instance (Eq e) => Eq (AnnotatedException e)

data TestException = TestException
    deriving (Eq, Show, Exception)

instance Eq SomeException where
    SomeException (e0 :: e0) == SomeException (e1 :: e1) =
        typeOf e0 == typeOf e1 && show e0 == show e1

instance Eq SomeAsyncException where
    SomeAsyncException (e0 :: e0) == SomeAsyncException (e1 :: e1) =
        typeOf e0 == typeOf e1 && show e0 == show e1

instance Eq ExceptionInLinkedThread where
    ExceptionInLinkedThread a0 e0 == ExceptionInLinkedThread a1 e1 =
        e0 == e1 && void a0 == void a1

instance Eq Safe.AsyncExceptionWrapper where
    Safe.AsyncExceptionWrapper e0 == Safe.AsyncExceptionWrapper e1 =
        typeOf e0 == typeOf e1 && show e0 == show e1

instance Eq UnliftIO.AsyncExceptionWrapper where
    UnliftIO.AsyncExceptionWrapper e0 == UnliftIO.AsyncExceptionWrapper e1 =
        typeOf e0 == typeOf e1 && show e0 == show e1

pass :: Expectation
pass = pure ()

emptyAnnotation :: e -> AnnotatedException e
emptyAnnotation = pure

spec :: Spec
spec = do
    describe "toException" $ do
        it "wraps inner in SomeException" $ do
            toException (AnnotatedException [] TestException)
                `shouldBe` do
                    SomeException
                        (AnnotatedException [] (SomeException TestException))
        it "flattens annotations" $ do
            let
                exn =
                    AnnotatedException ["hello"] $
                        AnnotatedException ["goodbye"] TestException
            toException exn
                `shouldBe` do
                    SomeException $
                        AnnotatedException ["hello", "goodbye"] (SomeException TestException)

    describe "AnnotatedException can fromException a" $ do
        it "different type" $ do
            fromException (toException TestException)
                `shouldBe`
                    Just (emptyAnnotation TestException)

        it "SomeException" $ do
            fromException (SomeException TestException)
                `shouldBe`
                    Just (emptyAnnotation (SomeException TestException))

        it "nested AnnotatedException" $ do
            fromException (toException (emptyAnnotation (emptyAnnotation TestException)))
                `shouldBe`
                    Just (emptyAnnotation TestException)

        it "can i guess also parse into a nested Annotated" $ do
            fromException (toException (emptyAnnotation TestException))
                `shouldBe`
                    Just (emptyAnnotation (emptyAnnotation TestException))

        it "does not loop infinitely if the wrong type is selected" $ do
            fromException (toException TestException)
                `shouldNotBe`
                    Just (emptyAnnotation $ userError "uh oh")

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

        it "permits other types to pass through" $ do
            let action =
                    Safe.throw (userError "uh oh")
                        `Safe.catch`
                            \(AnnotatedException _ TestException) ->
                                expectationFailure "Should not catch"
            action
                `shouldThrow`
                    (userError "uh oh" ==)

        describe "includes a callstack location" $ do
            it "with an originally annotated exception" $ do
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
            it "with a non-annotated original exception" $ do
                let
                    action =
                        Safe.throw TestException
                            `catch`
                                \TestException ->
                                    throw TestException
                action
                    `Safe.catch`
                            \(e :: AnnotatedException TestException) -> do
                                annotations e
                                    `callStackFunctionNamesShouldBe`
                                        [ "throw"
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

    describe "tryAnnotated" $ do
        let subject :: (Exception e, Exception e') => e -> IO (AnnotatedException e')
            subject exn = do
                Left exn' <- tryAnnotated (throw exn)
                pure exn'

        it "promotes to empty with no annotations" $ do
            exn <- subject TestException
            exn `shouldBeWithoutCallStackInAnnotations` AnnotatedException [] TestException

        it "preserves annotations" $ do
            exn <- subject $ AnnotatedException ["hello"] TestException
            exn `shouldBeWithoutCallStackInAnnotations` AnnotatedException ["hello"] TestException

        it "preserves annotations added via checkpoint" $ do
            Left exn <- tryAnnotated $ do
                checkpoint "hello" $ do
                    throw TestException
            exn `shouldBeWithoutCallStackInAnnotations`
                AnnotatedException ["hello"] TestException

        it "doesn't mess up if trying the wrong type" $ do
            let
                action = do
                    Left exn <- tryAnnotated $ do
                        checkpoint "hello" $ do
                            throw TestException
                    exn `shouldBe` AnnotatedException ["hello"] (userError "oh no")
            action `catch` \ann ->
                ann `shouldBeWithoutCallStackInAnnotations`
                    AnnotatedException ["hello"] TestException

    describe "throwWithCallstack" $ do
        it "includes a CallStack on the given exception" $ do
            throwWithCallStack TestException
                `shouldThrow`
                    isJust . annotatedExceptionCallStack @TestException
        describe "interaction with checkpointCallStack" $ do
            it "only has one CallStack" $ do
                let
                    action = do
                        checkpointCallStack $ do
                            throwWithCallStack TestException
                action
                    `Safe.catch` \(e :: AnnotatedException TestException) -> do
                        annotations e
                            `callStackFunctionNamesShouldBe`
                                [ "throwWithCallStack"
                                , "checkpointCallStack"
                                ]

    describe "try" $ do
        let subject :: (Exception e, Exception e') => e -> IO e'
            subject exn = do
                Left exn' <- try (throw exn)
                pure exn'

        describe "when throwing non-Annotated" $ do
            it "can add an empty annotation for a non-Annotated exception" $ do
                exn <- subject TestException
                exn `shouldBeWithoutCallStackInAnnotations` AnnotatedException [] TestException

            it "can catch a usual exception" $ do
                exn <- subject TestException
                exn `shouldBe` TestException

        describe "when throwing Annotated" $ do
            it "can catch a non-Annotated exception" $ do
                exn <- subject $ emptyAnnotation TestException
                exn `shouldBe` TestException

            it "can catch an Annotated exception" $ do
                exn <- subject TestException
                exn `shouldBeWithoutCallStackInAnnotations` emptyAnnotation TestException

        describe "when the wrong error is tried " $ do
            let
                boom :: IO a
                boom =
                    Safe.throwIO $ userError "uh oh"
            it "does not catch the exception" $ do
                let
                    scenario = do
                        eres <- try boom
                        case eres of
                            Left TestException ->
                                pure ()
                            Right () ->
                                pure ()
                scenario
                    `shouldThrow`
                        (\e -> userError "uh oh" == e) -- TestException

        describe "nesting behavior" $ do
            it "can catch at any level of nesting" $ do
                subject TestException
                    >>= (`shouldBeWithoutCallStackInAnnotations` emptyAnnotation TestException)
                subject TestException
                    >>= (`shouldBeWithoutCallStackInAnnotations` emptyAnnotation (emptyAnnotation TestException))
                subject TestException
                    >>= (`shouldBeWithoutCallStackInAnnotations` emptyAnnotation (emptyAnnotation (emptyAnnotation TestException)))

    describe "Safe.try" $ do
        it "can catch a located exception" $ do
            Left exn <- Safe.try (Safe.throw TestException)
            exn `shouldBe` emptyAnnotation TestException

        it "does not catch an AnnotatedException" $ do
            let action = do
                    Left exn <- Safe.try (Safe.throw $ emptyAnnotation TestException)
                    exn `shouldBe` TestException
            action `shouldThrow` (== emptyAnnotation TestException)

    describe "catches" $ do
        it "is exported" $ do
            let
                _x :: IO a -> [Handler IO a] -> IO a
                _x = catches
            pass


    describe "checkpoint" $ do
        it "adds annotations" $ do
            Left exn <- try (checkpoint "Here" (throw TestException))
            exn `shouldBeWithoutCallStackInAnnotations`
                AnnotatedException ["Here"] TestException

        it "adds two annotations" $ do
            Left exn <- try $ do
                checkpoint "Here" $ do
                    checkpoint "There" $ do
                        throw TestException
            exn `shouldBeWithoutCallStackInAnnotations`
                AnnotatedException ["Here", "There"] TestException

        it "adds three annotations" $ do
            Left exn <- try $
                checkpoint "Here" $
                checkpoint "There" $
                checkpoint "Everywhere" $
                throw TestException
            exn `shouldBeWithoutCallStackInAnnotations`
                AnnotatedException ["Here", "There", "Everywhere"] TestException

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
            exn `shouldBeWithoutCallStackInAnnotations`
                AnnotatedException ["Lmao"] TestException

        it "supports rethrowing" $ do
            Left exn <- try $
                checkpoint "A" $
                flip catch (\TestException -> throw TestException) $
                checkpoint "B" $
                throw TestException
            exn `shouldBeWithoutCallStackInAnnotations` AnnotatedException ["A", "B"] TestException

        it "handles CallStack nicely" $ do
            Left (AnnotatedException anns TestException) <- try $
                checkpoint (Annotation callStack) $
                    checkpoint (Annotation callStack) $
                        throwWithCallStack TestException

            anns `callStackFunctionNamesShouldBe`
                [ "throwWithCallStack"
                , "checkpoint"
                , "checkpoint"
                ]

    describe "HasCallStack behavior" $ do
        -- This section of the test suite exists to verify that some behavior
        -- acts how I expect it to. And/or learn how it behaves. Lol.
        let foo :: HasCallStack => IO ()
            foo = throwWithCallStack TestException
            bar :: HasCallStack => IO ()
            bar = foo
            baz :: HasCallStack => IO ()
            baz = bar

        it "should have source location" $ do
            foo
                `Safe.catch`
                    \(AnnotatedException anns TestException) -> do
                        anns
                            `callStackFunctionNamesShouldBe`
                                [ "throwWithCallStack"
                                , "foo"
                                ]

        it "appears to be throw-site first, then other entires" $ do
            baz
                `Safe.catch`
                    \(AnnotatedException anns TestException) -> do
                        anns
                            `callStackFunctionNamesShouldBe`
                                [ "throwWithCallStack"
                                , "foo"
                                , "bar"
                                , "baz"
                                ]

        describe "addCallstackToException" $ do
            let
                makeCs0 :: HasCallStack => IO CallStack
                makeCs0 = pure callStack
                makeCs1 :: HasCallStack => IO CallStack
                makeCs1 = pure callStack

            (cs0, cs1) <- runIO $ (,) <$> makeCs0 <*> makeCs1

            let baseException =
                    AnnotatedException [] TestException

            it "does not drop any other annotations" $ do
                addCallStackToException cs0 (AnnotatedException ["hello"] TestException)
                    `shouldBe`
                        AnnotatedException ["hello", Annotation cs0] TestException
            it "should add a CallStack to an empty AnnotatedException" $ do
                addCallStackToException cs0 baseException
                    `shouldBe`
                        AnnotatedException [Annotation cs0] TestException

            it "should not add a second CallStack to an AnnotatedException" $ do
                annotations (addCallStackToException cs1 (addCallStackToException cs0 baseException))
                    `shouldSatisfy` (1 ==) . length

            it "should merge CallStack as HasCallStack does" $ do
                [expectedAnnotation] <-
                    (undefined <$ foo) `Safe.catch`
                        \(AnnotatedException anns TestException) ->
                            pure anns
                Just expectedCallStack <- pure $ castAnnotation expectedAnnotation

                let
                    fooCS =
                        callStackFromFunctionName "foo"
                    throwWithCallStackCS =
                        callStackFromFunctionName "throwWithCallStack"
                    actualAnnotations =
                        annotations $
                            addCallStackToException fooCS  $
                                addCallStackToException
                                    throwWithCallStackCS
                                    baseException
                actualAnnotations
                    `callStackFunctionNamesShouldBe`
                        map fst (getCallStack expectedCallStack)

    describe "Async Exceptions" $ do
        describe "Async" $ do
            let
                withSideThread
                    :: (IO a -> (Async () -> IO ()) -> IO ())
                    -> (Async () -> IO ()) -> IO ()
                withSideThread myWithAsync k =  do
                    let
                        sideThread = do
                            checkpoint "hello" $ do
                                throwWithCallStack TestException
                    myWithAsync sideThread k

                waitTest myWait a =
                    myWait a
                        `assertOnCaughtException` \err -> do
                            err
                                `shouldBeWithoutCallStackInAnnotations`
                                    AnnotatedException ["hello"] TestException

                linkTest myLink a = do
                    x <- newEmptyMVar
                    myLink a
                    checkpoint "oh dang" (takeMVar x :: IO ())
                        `assertOnCaughtException` \outer@(AnnotatedException anns err) -> do
                            filterCallStack anns `shouldBe` ["oh dang"]
                            case err of
                                ExceptionInLinkedThread a' inner -> do
                                    asyncThreadId a' `shouldBe` asyncThreadId a
                                    let Just annE = fromException inner
                                    annE
                                        `shouldBeWithoutCallStackInAnnotations`
                                            AnnotatedException
                                            ["hello"]
                                            TestException

            let cases =
                    [ ("Control.Concurrent.Async", Async.withAsync, Async.wait, Async.link)
                    , ("UnliftIO.Async", UnliftIO.withAsync, UnliftIO.wait, UnliftIO.link)
                    ]

            forM_ cases $ \(moduleName, myWithAsync, myWait, myLink) -> do
                describe moduleName $ do
                    around (withSideThread myWithAsync) $ do
                        it "wait throws a sync exception" $
                            waitTest myWait
                        it "link throws async exception" $
                            linkTest myLink

            describe "cancel" $ do
                it "shows up in the thing" $ do
                    x <- newEmptyMVar :: IO (MVar SomeException)
                    w <- newEmptyMVar :: IO (MVar ())
                    let
                        sideThreadAction = do
                            (checkpoint "wow" $ do
                                putMVar w ()
                                takeMVar x)
                            `catch` \a -> do
                                putMVar x a
                                Control.Exception.throwIO a
                    Async.withAsync sideThreadAction $ \a -> do
                        takeMVar w
                        Async.cancel a
                        se <- takeMVar x
                        Just annE <- pure $ fromException se
                        annE `shouldBeWithoutCallStackInAnnotations`
                            AnnotatedException ["wow"] Async.AsyncCancelled
                        Async.wait a
                            `assertOnCaughtException` \annE ->
                                annE `shouldBeWithoutCallStackInAnnotations`
                                    AnnotatedException ["wow"] Async.AsyncCancelled

        describe "forkIO" $ do
            let
                setupSideThread myThrowTo = do
                    sideThreadWaitVar <- newEmptyMVar :: IO (MVar ())
                    sideThreadInstalledVar <- newEmptyMVar
                    sideThreadResultVar <- newEmptyMVar
                    let
                        sideThread =
                            checkpoint "hello" $ do
                                putMVar sideThreadInstalledVar ()
                                takeMVar sideThreadWaitVar
                    threadId <-
                        Control.Concurrent.forkFinally sideThread $ putMVar sideThreadResultVar
                    takeMVar sideThreadInstalledVar
                    myThrowTo threadId TestException
                    Left err@(SomeException someErr) <- takeMVar sideThreadResultVar
                    pure err

            describe "Control.Concurrent.throwTo" $ do
                before (setupSideThread Control.Concurrent.throwTo) $ do
                    it "has annotatedexception on the 'outer' layer" $ \(SomeException err) -> do
                        Just annE <- pure $ cast err
                        annE `shouldBeWithoutCallStackInAnnotations` do
                            AnnotatedException ["hello"] (SomeException TestException)
                    it "can transparently be fromException'ed" $ \someErr -> do
                        Just annE <- pure $ fromException someErr
                        annE `shouldBeWithoutCallStackInAnnotations` do
                            AnnotatedException ["hello"] TestException

            describe "UnliftIO.Concurrent.throwTo" $ do
                before (setupSideThread UnliftIO.Concurrent.throwTo) $ do
                    it "fromException only works on SomeAsyncException" $ \someErr -> do
                        Just annE <- pure $ fromException someErr
                        annE `shouldBeWithoutCallStackInAnnotations` do
                            AnnotatedException ["hello"] ((UnliftIO.AsyncExceptionWrapper TestException))
                    it "cast uses many wrappers" $ \(SomeException err) -> do
                        -- print $ typeOf err
                        Just (AnnotatedException anns (asyncWrapper :: SomeException)) <- pure $ cast err
                        filterCallStack anns `shouldBe` ["hello"]
                        SomeException err <- pure asyncWrapper
                        -- print $ typeOf err
                        Just (Safe.SomeAsyncException err) <- pure $ cast err
                        -- print $ typeOf err
                        Just (UnliftIO.AsyncExceptionWrapper err) <- pure $ cast err
                        -- print $ typeOf err
                        cast err `shouldBe` Just TestException

            describe "Control.Exception.Safe.throwTo" $ do
                before (setupSideThread Safe.throwTo) $ do
                    it "inner exception is wrapped" $ \someErr -> do
                        Just annE <- pure $ fromException someErr
                        annE `shouldBeWithoutCallStackInAnnotations` do
                            AnnotatedException ["hello"] (SomeAsyncException (Safe.AsyncExceptionWrapper TestException))
                    it "SomeAsyncException is transparent, AsyncExceptionWrapper is not" $ \someErr -> do
                        Just annE <- pure $ fromException someErr
                        annE `shouldBeWithoutCallStackInAnnotations` do
                            AnnotatedException ["hello"] (Safe.AsyncExceptionWrapper TestException)

assertOnCaughtException :: (Show a, Exception e) => IO a -> (e -> IO ()) -> IO ()
assertOnCaughtException action handler = do
    eres <- Control.Exception.try action
    case eres of
        Left err -> handler err
        Right a -> expectationFailure $ "Expected an error, got: " <> show a

callStackFunctionNamesShouldBe :: HasCallStack => [Annotation] -> [String] -> IO ()
callStackFunctionNamesShouldBe anns names = do
    let ([callStack], []) = tryAnnotations anns
    map fst (getCallStack callStack)
        `shouldBe`
            names

shouldBeWithoutCallStackInAnnotations
    :: (HasCallStack, Eq e, Show e, Exception e)
    => AnnotatedException e
    -> AnnotatedException e
    -> IO ()
shouldBeWithoutCallStackInAnnotations (AnnotatedException exp e0) e1 = do
    AnnotatedException (filterCallStack exp) e0 `shouldBe` e1

filterCallStack anns =
    snd $ tryAnnotations @CallStack anns

callStackFromFunctionName :: String -> CallStack
callStackFromFunctionName str =
    fromCallSiteList [(str, undefined)]
