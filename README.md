# ghc-typelits-extra

[![Build Status](https://secure.travis-ci.org/clash-lang/ghc-typelits-natnormalise.png?branch=master)](http://travis-ci.org/clash-lang/ghc-typelits-natnormalise)
[![Hackage](https://img.shields.io/hackage/v/ghc-typelits-natnormalise.svg)](https://hackage.haskell.org/package/ghc-typelits-natnormalise)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/ghc-typelits-natnormalise.svg?style=flat)](http://packdeps.haskellers.com/feed?needle=exact%3Aghc-typelits-natnormalise)

Extra type-level operations on GHC.TypeLits.Nat and a custom solver:

* `GHC.TypeLits.Extra.GCD`: a type-level `gcd`
* `GHC.TypeLits.Extra.CLog`: type-level equivalent of `clog x y = ceiling (logBase x y)`
