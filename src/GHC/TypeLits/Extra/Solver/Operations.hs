{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE MagicHash #-}

module GHC.TypeLits.Extra.Solver.Operations
  ( ExtraOp (..)
  , ExtraDefs (..)
  , reifyEOP
  , mergeMax
  , mergeMin
  , mergeDiv
  , mergeMod
  , mergeFLog
  , mergeCLog
  , mergeLog
  , mergeGCD
  , mergeLCM
  , mergeExp
  )
where

-- external
import Control.Monad.Trans.Writer.Strict

import GHC.Base                     (isTrue#,(==#),(+#))
import GHC.Integer                  (smallInteger)
import GHC.Integer.Logarithms       (integerLogBase#)
import GHC.TypeLits.Normalise.Unify (CType (..), normaliseNat, isNatural)

-- GHC API
import Outputable (Outputable (..), (<+>), integer, text)
import TcTypeNats (typeNatExpTyCon, typeNatSubTyCon)
import TyCon      (TyCon)
import Type       (Type, TyVar, mkNumLitTy, mkTyConApp, mkTyVarTy)

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

mergeMax :: ExtraDefs -> ExtraOp -> ExtraOp -> ExtraOp
mergeMax defs x y =
  let x' = reifyEOP defs x
      y' = reifyEOP defs y
      z  = fst (runWriter (normaliseNat (mkTyConApp typeNatSubTyCon [y',x'])))
      z' = fst <$> runWriterT (isNatural z)
  in  case z' of
        Just True  -> y
        Just False -> x
        _ -> Max x y

mergeMin :: ExtraDefs -> ExtraOp -> ExtraOp -> ExtraOp
mergeMin defs x y =
  let x' = reifyEOP defs x
      y' = reifyEOP defs y
      z  = fst (runWriter (normaliseNat (mkTyConApp typeNatSubTyCon [y',x'])))
      z' = fst <$> runWriterT (isNatural z)
  in  case z' of
        Just True  -> x
        Just False -> y
        _ -> Min x y

mergeDiv :: ExtraOp -> ExtraOp -> Maybe ExtraOp
mergeDiv _     (I 0)      = Nothing
mergeDiv (I i) (I j)      = Just (I (div i j))
mergeDiv x y              = Just (Div x y)

mergeMod :: ExtraOp -> ExtraOp -> Maybe ExtraOp
mergeMod _     (I 0)      = Nothing
mergeMod (I i) (I j)      = Just (I (mod i j))
mergeMod x y              = Just (Mod x y)

mergeFLog :: ExtraOp -> ExtraOp -> Maybe ExtraOp
mergeFLog (I i) _         | i < 2  = Nothing
mergeFLog i     (Exp j k) | i == j = Just k
mergeFLog (I i) (I j)              = I <$> flogBase i j
mergeFLog x     y                  = Just (FLog x y)

mergeCLog :: ExtraOp -> ExtraOp -> Maybe ExtraOp
mergeCLog (I i) _         | i < 2  = Nothing
mergeCLog i     (Exp j k) | i == j = Just k
mergeCLog (I i) (I j)              = I <$> clogBase i j
mergeCLog x     y                  = Just (CLog x y)

mergeLog :: ExtraOp -> ExtraOp -> Maybe ExtraOp
mergeLog (I i) _          | i < 2   = Nothing
mergeLog b     (Exp b' y) | b == b' = Just y
mergeLog (I i) (I j)                = I <$> exactLogBase i j
mergeLog x     y                    = Just (Log x y)

mergeGCD :: ExtraOp -> ExtraOp -> ExtraOp
mergeGCD (I i) (I j) = I (gcd i j)
mergeGCD x     y     = GCD x y

mergeLCM :: ExtraOp -> ExtraOp -> ExtraOp
mergeLCM (I i) (I j) = I (lcm i j)
mergeLCM x     y     = LCM x y

mergeExp :: ExtraOp -> ExtraOp -> ExtraOp
mergeExp (I i) (I j)                = I (i^j)
mergeExp b     (Log b' y) | b == b' = y
mergeExp x     y                    = Exp x y

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
