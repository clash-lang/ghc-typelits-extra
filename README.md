# ghc-typelits-extra

[![Build Status](https://secure.travis-ci.org/clash-lang/ghc-typelits-extra.png?branch=master)](http://travis-ci.org/clash-lang/ghc-typelits-extra)
[![Hackage](https://img.shields.io/hackage/v/ghc-typelits-extra.svg)](https://hackage.haskell.org/package/ghc-typelits-extra)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/ghc-typelits-extra.svg?style=flat)](http://packdeps.haskellers.com/feed?needle=exact%3Aghc-typelits-extra)

Extra type-level operations on GHC.TypeLits.Nat and a custom solver implemented
as a GHC type-checker plugin:

* `GHC.TypeLits.Extra.Max`: type-level [max](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#v:max)
* `GHC.TypeLits.Extra.Min`: type-level [min](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#v:min)
* `GHC.TypeLits.Extra.Div`: type-level [div](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#v:div)
* `GHC.TypeLits.Extra.Mod`: type-level [mod](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#v:mod)
* `GHC.TypeLits.Extra.FLog`: type-level equivalent of [integerLogBase#](https://hackage.haskell.org/package/integer-gmp/docs/GHC-Integer-Logarithms.html#v:integerLogBase-35-)
   .i.e. the exact integer equivalent to "`floor (logBase x y)`"
* `GHC.TypeLits.Extra.CLog`: type-level equivalent of _the ceiling of_ [integerLogBase#](https://hackage.haskell.org/package/integer-gmp/docs/GHC-Integer-Logarithms.html#v:integerLogBase-35-)
   .i.e. the exact integer equivalent to "`ceiling (logBase x y)`"
* 'GHC.TypeLits.Extra.Log': type-level equivalent of <https://hackage.haskell.org/package/integer-gmp/docs/GHC-Integer-Logarithms.html#v:integerLogBase-35- integerLogBase#>
     where the operation only reduces when "`floor (logBase b x) ~ ceiling (logBase b x)`"
* `GHC.TypeLits.Extra.GCD`: a type-level [gcd](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#v:gcd)
* `GHC.TypeLits.Extra.LCM`: a type-level [lcm](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#v:lcm)
