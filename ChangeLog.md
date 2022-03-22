# Changelog for located-exception

## Unreleased changes

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
