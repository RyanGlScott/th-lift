# Change Log

All notable changes to this project will be documented in this file.

## [0.8.2] - 2020-09-29

* Allow building with `template-haskell-2.17.0.0` (GHC 9.0).
* Make `deriveLift` work for unlifted newtypes.

## [0.8.1] - 2019-12-06

* Support GHC 8.10/`template-haskell-2.16`.
* Derive implementations of `liftTyped` (in addition to `lift`) when using
  `template-haskell-2.16` or later.
* Fix a bug in which derived `Lift` instances for data types containing
  `Addr#` would fail to typecheck.

## [0.8.0.1] - 2019-05-09

* Support GHC 8.8/`template-haskell-2.15`.

## [0.8] - 2019-04-26

* Remove `Lift ()`, `Ratio`, `Identity` and `Const ()` instances.
  These are now provided in [`th-lift-instances` package](http://hackage.haskell.org/package/th-lift-instances)
- Use `TemplateHaskellQuotes` where available

## [0.7.11] - 2018-08-27

* Support for GHC 8.6.

## [0.7.10] - 2018-02-01

* Add support for data families

## [0.7.9] - 2018-02-01

* `Lift` instances for `Identity` and `Const`.

## [0.7.8] - 2018-02-01

* GHC 8.4 compatibility.

## [0.7.7] - 2015-04-19

* GHC 8.2 compatibility.

## [0.7.6] - 2015-01-18

* GHC 8.1 compatibility.

## [0.7.5] - 2015-11-19

* Added `makeLift`, for cases when it's necessary to write the
  instance manually. (Thanks to Ryan Scott).
* Support empty datatypes and unboxed types.

## [0.7] - 2014-12-07

* Support GHC 7.9 and hopefully 7.10, thanks to Richard Eisenberg.
* On versions of GHC that support role inference, don't constrain
  phantom type variables.
* Get rid of some orphan instances when using GHC >= 7.9.

## [0.6] - 2013-12-09

* Support GHC 7.8, thanks to Michael Snoyberg.
* Support existentially quantified type variables in datatype
  declarations.
* Handle exotic kinds properly.

## [0.5.2] - 2010-09-19

* Support older GHCs and Cabal, thanks to Ben Millwood.

## [0.5] - 2010-08-05

* Support for contexts in datatypes, thanks to Ben Millwood.
* `deriveLiftWith` becomes `deriveLift'` and takes an `Info`
  structure rather than a custom reification function.
* Add `deriveLiftMany` to derive many `Lift` instances in one go.

## [0.4] - 2010-08-02

* Add support for newtypes and records syntax, thanks to a patch by
  Ben Millwood.
* Add support for infix constructors.
* `deriveLift` returns a list of declarations.
* New `deriveLiftWith` function with custom reification, following
  a feature request by Jonas Duregård.

## [0.3] - 2010-03-24

* Port to Template Haskell 2.4.
* Maintainer is now Mathieu Boespflug.

## [0.2] - 2006-09-06

Initial release by Ian Lynagh.
