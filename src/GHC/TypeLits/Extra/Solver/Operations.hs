{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

module GHC.TypeLits.Extra.Solver.Operations
  ( ExtraOp (..)
  , ExtraDefs (..)
  , Normalised (..)
  , NormaliseResult
  , mergeNormalised
  , reifyEOP
  , mergeMax
  , mergeMin
  , mergeDiv
  , mergeMod
  , mergeFLog
  , mergeCLog
  , mergeCLogWZ
  , mergeLog
  , mergeGCD
  , mergeLCM
  , mergeExp
  )
where

-- external
import Control.Monad.Trans.Writer.Strict
#if MIN_VERSION_ghc_typelits_natnormalise(0,7,0)
import Data.Set                     as Set
#endif

import GHC.Base                     (isTrue#,(==#),(+#))
import GHC.Integer                  (smallInteger)
import GHC.Integer.Logarithms       (integerLogBase#)
import GHC.TypeLits.Normalise.Unify (CType (..), normaliseNat, isNatural)

-- GHC API
#if MIN_VERSION_ghc(9,0,0)
import GHC.Builtin.Types.Literals (typeNatExpTyCon, typeNatSubTyCon)
import GHC.Core.TyCon (TyCon)
import GHC.Core.Type (Type, TyVar, mkNumLitTy, mkTyConApp, mkTyVarTy)
import GHC.Utils.Outputable (Outputable (..), (<+>), integer, text)
#else
import Outputable (Outputable (..), (<+>), integer, text)
import TcTypeNats (typeNatExpTyCon, typeNatSubTyCon)
import TyCon      (TyCon)
import Type       (Type, TyVar, mkNumLitTy, mkTyConApp, mkTyVarTy)
#endif

-- | Indicates whether normalisation has occured
data Normalised = Normalised | Untouched
  deriving Eq

instance Outputable Normalised where
  ppr Normalised = text "Normalised"
  ppr Untouched  = text "Untouched"

mergeNormalised :: Normalised -> Normalised -> Normalised
mergeNormalised Normalised _ = Normalised
mergeNormalised _ Normalised = Normalised
mergeNormalised _ _          = Untouched

-- | A normalise result contains the ExtraOp and a flag that indicates whether any expression
-- | was normalised within the ExtraOp.
type NormaliseResult = (ExtraOp, Normalised)

data ExtraOp
  = I    Integer
  | V    TyVar
  | C    CType
  | Max  ExtraOp ExtraOp
  | Min  ExtraOp ExtraOp
  | Div  ExtraOp ExtraOp
  | Mod  ExtraOp ExtraOp
  | FLog ExtraOp ExtraOp
  | CLog ExtraOp ExtraOp
  | Log  ExtraOp ExtraOp
  | GCD  ExtraOp ExtraOp
  | LCM  ExtraOp ExtraOp
  | Exp  ExtraOp ExtraOp
  | CLogWZ ExtraOp ExtraOp ExtraOp
  deriving Eq

instance Outputable ExtraOp where
  ppr (I i)      = integer i
  ppr (V v)      = ppr v
  ppr (C c)      = ppr c
  ppr (Max x y)  = text "Max (" <+> ppr x <+> text "," <+> ppr y <+> text ")"
  ppr (Min x y)  = text "Min (" <+> ppr x <+> text "," <+> ppr y <+> text ")"
  ppr (Div x y)  = text "Div (" <+> ppr x <+> text "," <+> ppr y <+> text ")"
  ppr (Mod x y)  = text "Mod (" <+> ppr x <+> text "," <+> ppr y <+> text ")"
  ppr (FLog x y) = text "FLog (" <+> ppr x <+> text "," <+> ppr y <+> text ")"
  ppr (CLog x y) = text "CLog (" <+> ppr x <+> text "," <+> ppr y <+> text ")"
  ppr (Log x y)  = text "Log (" <+> ppr x <+> text "," <+> ppr y <+> text ")"
  ppr (GCD x y)  = text "GCD (" <+> ppr x <+> text "," <+> ppr y <+> text ")"
  ppr (LCM x y)  = text "GCD (" <+> ppr x <+> text "," <+> ppr y <+> text ")"
  ppr (Exp x y)  = text "Exp (" <+> ppr x <+> text "," <+> ppr y <+> text ")"
  ppr (CLogWZ x y z) =
    text "CLogWZ "
      <+> text "(" <+> ppr x
      <+> text "," <+> ppr y
      <+> text "," <+> ppr z
      <+> text ")"

data ExtraDefs = ExtraDefs
  { maxTyCon  :: TyCon
  , minTyCon  :: TyCon
  , divTyCon  :: TyCon
  , modTyCon  :: TyCon
  , flogTyCon :: TyCon
  , clogTyCon :: TyCon
  , logTyCon  :: TyCon
  , gcdTyCon  :: TyCon
  , lcmTyCon  :: TyCon
  , ordTyCon  :: TyCon
  , assertTC  :: TyCon
  , clogWZTyCon :: TyCon
  }

reifyEOP :: ExtraDefs -> ExtraOp -> Type
reifyEOP _ (I i) = mkNumLitTy i
reifyEOP _ (V v) = mkTyVarTy v
reifyEOP _ (C (CType c)) = c
reifyEOP defs (Max x y)  = mkTyConApp (maxTyCon defs)  [reifyEOP defs x
                                                       ,reifyEOP defs y]
reifyEOP defs (Min x y)  = mkTyConApp (minTyCon defs)  [reifyEOP defs x
                                                       ,reifyEOP defs y]
reifyEOP defs (Div x y)  = mkTyConApp (divTyCon defs)  [reifyEOP defs x
                                                       ,reifyEOP defs y]
reifyEOP defs (Mod x y)  = mkTyConApp (modTyCon defs)  [reifyEOP defs x
                                                       ,reifyEOP defs y]
reifyEOP defs (CLog x y) = mkTyConApp (clogTyCon defs) [reifyEOP defs x
                                                       ,reifyEOP defs y]
reifyEOP defs (CLogWZ x y z) = mkTyConApp (clogTyCon defs)
  $ reifyEOP defs <$> [x, y, z]
reifyEOP defs (FLog x y) = mkTyConApp (flogTyCon defs) [reifyEOP defs x
                                                       ,reifyEOP defs y]
reifyEOP defs (Log x y)  = mkTyConApp (logTyCon defs)  [reifyEOP defs x
                                                       ,reifyEOP defs y]
reifyEOP defs (GCD x y)  = mkTyConApp (gcdTyCon defs)  [reifyEOP defs x
                                                       ,reifyEOP defs y]
reifyEOP defs (LCM x y)  = mkTyConApp (lcmTyCon defs)  [reifyEOP defs x
                                                       ,reifyEOP defs y]
reifyEOP defs (Exp x y)  = mkTyConApp typeNatExpTyCon  [reifyEOP defs x
                                                       ,reifyEOP defs y]

mergeMax :: ExtraDefs -> ExtraOp -> ExtraOp -> NormaliseResult
mergeMax _ (I 0) y = (y, Normalised)
mergeMax _ x (I 0) = (x, Normalised)
mergeMax defs x y =
  let x' = reifyEOP defs x
      y' = reifyEOP defs y
      z  = fst (runWriter (normaliseNat (mkTyConApp typeNatSubTyCon [y',x'])))
#if MIN_VERSION_ghc_typelits_natnormalise(0,7,0)
  in  case runWriterT (isNatural z) of
        Just (True , cs) | Set.null cs -> (y, Normalised)
        Just (False, cs) | Set.null cs -> (x, Normalised)
#else
  in  case isNatural z of
        Just True  -> (y, Normalised)
        Just False -> (x, Normalised)
#endif
        _ -> (Max x y, Untouched)

mergeMin :: ExtraDefs -> ExtraOp -> ExtraOp -> NormaliseResult
mergeMin defs x y =
  let x' = reifyEOP defs x
      y' = reifyEOP defs y
      z  = fst (runWriter (normaliseNat (mkTyConApp typeNatSubTyCon [y',x'])))
#if MIN_VERSION_ghc_typelits_natnormalise(0,7,0)
  in  case runWriterT (isNatural z) of
        Just (True, cs) | Set.null cs -> (x, Normalised)
        Just (False,cs) | Set.null cs -> (y, Normalised)
#else
  in  case isNatural z of
        Just True  -> (x, Normalised)
        Just False -> (y, Normalised)
#endif
        _ -> (Min x y, Untouched)

mergeDiv :: ExtraOp -> ExtraOp -> Maybe NormaliseResult
mergeDiv _     (I 0)      = Nothing
mergeDiv (I i) (I j)      = Just (I (div i j), Normalised)
mergeDiv x y              = Just (Div x y, Untouched)

mergeMod :: ExtraOp -> ExtraOp -> Maybe NormaliseResult
mergeMod _     (I 0)      = Nothing
mergeMod (I i) (I j)      = Just (I (mod i j), Normalised)
mergeMod x y              = Just (Mod x y, Untouched)

mergeFLog :: ExtraOp -> ExtraOp -> Maybe NormaliseResult
mergeFLog (I i) _         | i < 2  = Nothing
mergeFLog i     (Exp j k) | i == j = Just (k, Normalised)
mergeFLog (I i) (I j)              = fmap (\r -> (I r, Normalised)) (flogBase i j)
mergeFLog x     y                  = Just (FLog x y, Untouched)

mergeCLog :: ExtraOp -> ExtraOp -> Maybe NormaliseResult
mergeCLog (I i) _         | i < 2  = Nothing
mergeCLog i     (Exp j k) | i == j = Just (k, Normalised)
mergeCLog (I i) (I j)              = fmap (\r -> (I r, Normalised)) (clogBase i j)
mergeCLog x     y                  = Just (CLog x y, Untouched)

mergeCLogWZ :: ExtraOp -> ExtraOp -> ExtraOp -> Maybe NormaliseResult
mergeCLogWZ (I i) _         _ | i < 2  = Nothing
mergeCLogWZ _     (I 0)     z          = Just (z, Normalised)
mergeCLogWZ i     (Exp j k) _ | i == j = Just (k, Normalised)
mergeCLogWZ x     y@(I _)   _          = do (res, _) <- mergeCLog x y
                                            pure (res, Normalised)
mergeCLogWZ x     y         z          = Just (CLogWZ x y z, Untouched)

mergeLog :: ExtraOp -> ExtraOp -> Maybe NormaliseResult
mergeLog (I i) _          | i < 2   = Nothing
mergeLog b     (Exp b' y) | b == b' = Just (y, Normalised)
mergeLog (I i) (I j)                = fmap (\r -> (I r, Normalised)) (exactLogBase i j)
mergeLog x     y                    = Just (Log x y, Untouched)

mergeGCD :: ExtraOp -> ExtraOp -> NormaliseResult
mergeGCD (I i) (I j) = (I (gcd i j), Normalised)
mergeGCD x     y     = (GCD x y, Untouched)

mergeLCM :: ExtraOp -> ExtraOp -> NormaliseResult
mergeLCM (I i) (I j) = (I (lcm i j), Normalised)
mergeLCM x     y     = (LCM x y, Untouched)

mergeExp :: ExtraOp -> ExtraOp -> NormaliseResult
mergeExp (I i) (I j)                = (I (i^j), Normalised)
mergeExp b     (Log b' y) | b == b' = (y, Normalised)
mergeExp x     y                    = (Exp x y, Untouched)

-- | \x y -> logBase x y, x > 1 && y > 0
flogBase :: Integer -> Integer -> Maybe Integer
flogBase x y | y > 0 = Just (smallInteger (integerLogBase# x y))
flogBase _ _         = Nothing

-- | \x y -> ceiling (logBase x y), x > 1 && y > 0
clogBase :: Integer -> Integer -> Maybe Integer
clogBase x y | y > 0 =
  let z1 = integerLogBase# x y
      z2 = integerLogBase# x (y-1)
  in  case y of
         1 -> Just 0
         _ | isTrue# (z1 ==# z2) -> Just (smallInteger (z1 +# 1#))
           | otherwise           -> Just (smallInteger z1)
clogBase _ _ = Nothing

-- | \x y -> logBase x y, x > 1 && y > 0, logBase x y == ceiling (logBase x y)
exactLogBase :: Integer -> Integer -> Maybe Integer
exactLogBase x y | y > 0 =
  let z1 = integerLogBase# x y
      z2 = integerLogBase# x (y-1)
  in  case y of
        1 -> Just 0
        _ | isTrue# (z1 ==# z2) -> Nothing
          | otherwise           -> Just (smallInteger z1)
exactLogBase _ _ = Nothing
