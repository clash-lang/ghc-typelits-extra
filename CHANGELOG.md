# Changelog for the [`ghc-typelits-extra`](http://hackage.haskell.org/package/ghc-typelits-extra) package

# 0.2.2 *January 15th 2017*
* Reduce `Min n (n+1)` to `n`
* Reduce `Max n (n+1)` to `n+1`
* Reduce cases like `1 <=? Div 18 6` to `True`
* Add a type-level division that rounds up: `type DivRU n d = Div (n + (d - 1)) d`
* Add a type-level `divMod` : `DivMod :: Nat -> Nat -> '(Nat, Nat)`

# 0.2.1 *September 29th 2016*
* Reduce `Max n n` to `n`
* Reduce `Min n n` to `n`

# 0.2 *August 19th 2016*
* New type-level operations:
  * `Max`: type-level `max`
  * `Min`: type-level `min`
  * `Div`: type-level `div`
  * `Mod`: type-level `mod`
  * `FLog`: floor of logBase
  * `Log`: exact integer logBase (i.e. where `floor (logBase b x) ~ ceiling (logBase b x)` holds)
  * `LCM`: type-level `lcm`
* Fixes bugs:
  * `CLog b 1` doesn't reduce to `0`

## 0.1.3 *July 19th 2016*
* Fixes bugs:
  * Rounding error in `CLog` calculation

## 0.1.2 *July 8th 2016*
* Solve KnownNat constraints over CLog and GCD, i.e., KnownNat (CLog 2 4)

## 0.1.1 *January 20th 2016*
* Compile on GHC 8.0+

## 0.1 *October 21st 2015*
* Initial release
