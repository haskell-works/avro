# Revision history for avro

## HEAD

- Replace `entropy` library with `tf-random`, which is easier to use with ghcjs

## 0.4.1.1

- Fixed bugs in handling of namespaces when parsing and printing avro types
- Fixed a schema overlay test

## 0.4.1.0

- Fixed an omitted data fixture from the cabal sdist
- Improvements on experimental lazy decoding (up to 25% faster on our tests)
- Useful instances for EitherN

## 0.4.0.0

- Technical release to respect potentially breaking changes introduced earlier.

## 0.3.6.1

- Fixed Data.Avro.Schema.extractBindings by @TikhonJelvis

## 0.1.0.0  -- YYYY-mm-dd

- First version. Released on an unsuspecting world.
