# `annotated-exception`

This library provides a special `AnnotatedException` type which allows you to decorate Haskell exceptions with additional information.
This decoration is totally transparent, and even works with exceptions thrown outside of your application code.

To provide an annotation, you'd use the function `checkpoint`.
This will attach the provided value to any exception that bubbles up through it.

```haskell
import Control.Exception.Annotated 

data MyException = MyException 
    deriving (Show, Exception)

main :: IO ()
main = do
    checkpoint "Foo" $ do
        throw MyException
```

When this program crashes, it will crash with an `AnnotatedException` that contains the annotation `"Foo"`.

```
λ> checkpoint "Foo" $ throw MyException
*** Exception: AnnotatedException {annotations = ["Foo"], exception = MyException}
```

These annotations survive, even if you catch and rethrow with a different exception.

```haskell
data OtherException = OtherException
    deriving (Show, Exception)

woah :: IO ()
woah = do
    let 
        checkpointed = 
            checkpoint "Foo" (throw MyException)
        handler MyException =
            throw OtherException

    checkpointed
        `catch`
            handler

```

Notice how the `checkpoint` call doesn't cover the `throw OtherException` - the exception `[Annotation]` lives on the thrown exception itself, and this library's `catch` function ensures that we don't lose that context.

```
λ> (checkpoint "Foo" (throw MyException)) `catch` \MyException -> throw OtherException
*** Exception: AnnotatedException {annotations = ["Foo"], exception = OtherException}
```

Now, you're about to report your exceptions, up near `main`.
We can use `try` in this module to always get the annotations.

```haskell
main = do
    eresult <- try $ myProgram
    case eresult of
        Left (AnnotatedException annotations exception) ->
            reportException annotations exception
        Right a ->
            pure a
```
