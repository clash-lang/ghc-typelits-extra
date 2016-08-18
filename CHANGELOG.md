# Changelog for the [`ghc-typelits-extra`](http://hackage.haskell.org/package/ghc-typelits-extra) package

# 0.2
* New type-level operations:
  * `Max`: type-level `max`
  * `Min`: type-level `min`
  * `Div`: type-level `div`
  * `Mod`: type-level `mod`
  * `LCM`: type-level `lcm`

## 0.1.3 *July 19th 2016*
* Fixes bugs:
  * Rounding error in `CLog` calculation

## 0.1.2 *July 8th 2016*
* Solve KnownNat constraints over CLog and GCD, i.e., KnownNat (CLog 2 4)

## 0.1.1 *January 20th 2016*
* Compile on GHC 8.0+

## 0.1 *October 21st 2015*
* Initial release
