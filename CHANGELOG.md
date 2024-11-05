## [*Unreleased*](https://github.com/pbrisbin/hs-shellwords/compare/v0.1.3.2...main)

## [v0.1.3.2](https://github.com/pbrisbin/hs-shellwords/compare/v0.1.3.1...v0.1.3.2)

- Fix bugs around `=` handling, adjacent strings, and escaped-backslash

## [v0.1.3.1](https://github.com/pbrisbin/hs-shellwords/compare/v0.1.3.0...v0.1.3.1)

- Fix incorrect lower bound on `base`

## [v0.1.3.0](https://github.com/pbrisbin/hs-shellwords/compare/v0.1.2.1...v0.1.3.0)

- Define reserved characters, to enable delimited parsing `$(<words)`
- Export `Parser`-related functions, to enable incorporating in a larger parser

## [v0.1.2.1](https://github.com/pbrisbin/hs-shellwords/compare/v0.1.2.0...v0.1.2.1)

- Strip surrounding whitespace before parsing
- Fix mis-handling of escaped spaces in certain kinds of flags

## [v0.1.2.0](https://github.com/pbrisbin/hs-shellwords/compare/v0.1.1.0...v0.1.2.0)

- `parse` works on `String` now, use `parseText` for the `Text` interface

## [v0.1.1.0](https://github.com/pbrisbin/hs-shellwords/compare/v0.1.0.0...v0.1.1.0)

- Bugfixes that I can't remember

## [v0.1.0.0](https://github.com/pbrisbin/hs-shellwords/tree/v0.1.0.0)

First released version.
