# Changelog [![kraklin/elm-debug-parser](https://img.shields.io/elm-package/v/kraklin/elm-debug-parser.svg)](https://package.elm-lang.org/packages/kraklin/elm-debug-parser/latest/)

All notable changes to
[the `kraklin/elm-debug-parser` elm package](http://package.elm-lang.org/packages/kraklin/elm-debug-parser/latest)
will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to
[Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [2.0.0]
  Changed output type to be defined by the config instead of being hardcoded to ElmValue.

### Removed
  - `parseWithOptionalTag` - it was replaced by a function 

### Added 
  - `parseValue` parses only the part of the debug log after the colon, e.g. output of `Debug.toString`
  - `Config` type alias for record with function definitions for transformations on the parsed elements
  - `defaultConfig` to keep the default `ElmValue` implementation of output

### Changed
  - `parse` now needs a `Config` to be specified

## [1.0.0]
  - initial release
