# Changelog for located-exception

## Unreleased changes

## 0.2.0.0

- [#12](https://github.com/parsonsmatt/annotated-exception/pull/12)
    - Removed the `Eq` instance for `Annotation` as well as the `Eq` constraint
      in `AnnC`. These instances were only used for testing, and prevented the
      natural use of `CallStack` in a `[Annotation]`.
    - Removed the `Eq` instance for `AnnotatedException` as a consequence of
      dropping the `Eq` instance on `Annotation`.
    - Removed the `new` function. Use `pure` or `exceptionWithCallStack` instead.
    - Fixed a double-annotation bug in `checkpointCallStackWith`.
    - `checkpointCallStack` appends to the call-site list.
    - Pretty much everything now merges the `CallStack`s together. `throw`
      includes a `CallStack`, as do `checkpoint` and `checkpointMany`.

## 0.1.2.1

- [#8](https://github.com/parsonsmatt/annotated-exception/pull/8)
    - There was a bug where catching or trying to catch an exception of the
      wrong type would trigger an infinite loop as the `fromException` method
      kept digging and digging and would be unable to make things work out. The
      `fromException` code no longer tries to flatten out these exceptions.
      However, `toException` *does* flatten it, so all tests still pass.

## 0.1.2.0

- [#6](https://github.com/parsonsmatt/annotated-exception/pull/6)
    - Add `Control.Exception.Annotated.UnliftIO` that uses `MonadUnliftIO`
      instead of `MonadCatch` and `MonadThrow`.
    - Actually expose `catches`

## 0.1.1.0

- [#4](https://github.com/parsonsmatt/annotated-exception/pull/4)
    - Add `catches`
    - Replace `Control.Exception.Safe.try` with `try` that can get an
      `AnnotatedException e` or a regular, un-`Annotated` `e`.

## 0.1.0.0

- Initial Release
