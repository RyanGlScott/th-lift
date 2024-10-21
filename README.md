# `th-lift`
[![Hackage](https://img.shields.io/hackage/v/th-lift.svg)][Hackage: th-lift]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/th-lift.svg)](http://packdeps.haskellers.com/reverse/th-lift)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![Build Status](https://github.com/RyanGlScott/th-lift/workflows/Haskell-CI/badge.svg)](https://github.com/RyanGlScott/th-lift/actions?query=workflow%3AHaskell-CI)

[Hackage: th-lift]:
  http://hackage.haskell.org/package/th-lift
  "th-lift package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"

Derive Template Haskell's `Lift` class for datatypes using `TemplateHaskell`.
The functionality in this package has largely been subsumed by the
`DeriveLift` language extension, which is available in GHC 8.0 and later
versions. As such, this package is only useful as a way to backport bugfixes
to `DeriveLift` in later GHC versions back to older GHCs.

The following libraries are related:

* The [th-orphans](https://hackage.haskell.org/package/th-orphans) package
  provides instances for `template-haskell` syntax types.

* The [th-lift-instances](http://hackage.haskell.org/package/th-lift-instances)
  package provides `Lift` instances for types in `base`, `text`,
  `bytestring`, `vector`, etc. Some of these instances are only provided for
  old versions of their respective libraries, as the same `Lift` instances
  are also present upstream on newer versions.
