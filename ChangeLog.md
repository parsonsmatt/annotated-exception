# Changelog for located-exception

## Unreleased changes

## 0.1.2.0

- [#]()
    - Add `Control.Exception.Annotated.UnliftIO` that uses `MonadUnliftIO`
      instead of `MonadCatch` and `MonadThrow`.

## 0.1.1.0

- [#4](https://github.com/parsonsmatt/annotated-exception/pull/4)
    - Add `catches`
    - Replace `Control.Exception.Safe.try` with `try` that can get an
      `AnnotatedException e` or a regular, un-`Annotated` `e`.

## 0.1.0.0

- Initial Release
