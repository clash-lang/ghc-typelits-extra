# ghc-typelits-extra

[![Build Status](https://secure.travis-ci.org/clash-lang/ghc-typelits-extra.png?branch=master)](http://travis-ci.org/clash-lang/ghc-typelits-extra)
[![Hackage](https://img.shields.io/hackage/v/ghc-typelits-extra.svg)](https://hackage.haskell.org/package/ghc-typelits-extra)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/ghc-typelits-extra.svg?style=flat)](http://packdeps.haskellers.com/feed?needle=exact%3Aghc-typelits-extra)

Extra type-level operations on GHC.TypeLits.Nat and a custom solver implemented
as a GHC type-checker plugin:

* `GHC.TypeLits.Extra.GCD`: a type-level `gcd`
* `GHC.TypeLits.Extra.CLog`: type-level equivalent of _the ceiling of_ [integerLogBase#](https://hackage.haskell.org/package/integer-gmp/docs/GHC-Integer-Logarithms.html#v:integerLogBase-35-)
