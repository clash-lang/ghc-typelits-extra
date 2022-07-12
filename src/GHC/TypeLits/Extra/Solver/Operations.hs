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
  , toExtraOp
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
#if MIN_VERSION_ghc_typelits_natnormalise(0,7,0)
import Data.Set                     as Set
#endif

import GHC.Base                     (isTrue#,(==#),(+#))
import GHC.Integer                  (smallInteger)
import GHC.Integer.Logarithms       (integerLogBase#)

import qualified GHC.TypeLits.Normalise.SOP as N
  (SOP (..), Product(..), Symbol(..))
import qualified GHC.TypeLits.Normalise.Unify as N
  (CType (..), normaliseNat, isNatural, reifySOP)

-- GHC API
#if MIN_VERSION_ghc(9,0,0)
import GHC.Builtin.Types.Literals (typeNatExpTyCon, typeNatAddTyCon
                                  , typeNatSubTyCon)
import GHC.Core.TyCon (TyCon)
import GHC.Core.TyCo.Rep (Type (..), TyLit (..))
import GHC.Core.Type (TyVar, mkNumLitTy, mkTyConApp, mkTyVarTy, coreView)
import GHC.Utils.Outputable (Outputable (..), (<+>), integer, text)
#else
import Outputable (Outputable (..), (<+>), integer, text)
import TcTypeNats (typeNatExpTyCon, typeNatAddTyCon, typeNatSubTyCon)
import TyCon      (TyCon)
import TyCoRep    (Type (..), TyLit (..))
import Type       (Type, TyVar, mkNumLitTy, mkTyConApp, mkTyVarTy, coreView)
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

toExtraOp :: ExtraDefs -> Type -> Maybe ExtraOp
toExtraOp defs ty | Just ty1 <- coreView ty = toExtraOp defs ty1
toExtraOp _ (TyVarTy v)          = pure (V v)
toExtraOp _ (LitTy (NumTyLit i)) = pure (I i)
toExtraOp defs (TyConApp tc [x,y])
  | tc == maxTyCon defs = Max <$> (toExtraOp defs x) <*> (toExtraOp defs y)
  | tc == minTyCon defs = Min <$> (toExtraOp defs x) <*> (toExtraOp defs y)
  | tc == divTyCon defs = Div <$> (toExtraOp defs x) <*> (toExtraOp defs y)
  | tc == modTyCon defs = Mod <$> (toExtraOp defs x) <*> (toExtraOp defs y)
  | tc == flogTyCon defs = FLog <$> (toExtraOp defs x) <*> (toExtraOp defs y)
  | tc == clogTyCon defs = CLog <$> (toExtraOp defs x) <*> (toExtraOp defs y)
  | tc == logTyCon defs = Log <$> (toExtraOp defs x) <*> (toExtraOp defs y)
  | tc == gcdTyCon defs = GCD <$> (toExtraOp defs x) <*> (toExtraOp defs y)
  | tc == lcmTyCon defs = LCM <$> (toExtraOp defs x) <*> (toExtraOp defs y)
  | tc == typeNatExpTyCon = Exp <$> (toExtraOp defs x) <*> (toExtraOp defs y)
toExtraOp _ t = Just (C (N.CType t))

-- | A normalise result contains the ExtraOp and a flag that indicates whether any expression
-- | was normalised within the ExtraOp.
type NormaliseResult = (ExtraOp, Normalised)

data ExtraOp
  = I    Integer
  | V    TyVar
  | C    N.CType
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
  , ordTyCon  :: TyCon
  }

reifyEOP :: ExtraDefs -> ExtraOp -> Type
reifyEOP _ (I i) = mkNumLitTy i
reifyEOP _ (V v) = mkTyVarTy v
reifyEOP _ (C (N.CType c)) = c
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

mergeMax :: ExtraDefs -> ExtraOp -> ExtraOp -> NormaliseResult
mergeMax _ (I 0) y = (y, Normalised)
mergeMax _ x (I 0) = (x, Normalised)
mergeMax defs x y =
  let x' = reifyEOP defs x
      y' = reifyEOP defs y
      z  = fst (runWriter (N.normaliseNat (mkTyConApp typeNatSubTyCon [y',x'])))
#if MIN_VERSION_ghc_typelits_natnormalise(0,7,0)
  in  case runWriterT (N.isNatural z) of
        Just (True , cs) | Set.null cs -> (y, Normalised)
        Just (False, cs) | Set.null cs -> (x, Normalised)
#else
  in  case N.isNatural z of
        Just True  -> (y, Normalised)
        Just False -> (x, Normalised)
#endif
        _ -> (Max x y, Untouched)

mergeMin :: ExtraDefs -> ExtraOp -> ExtraOp -> NormaliseResult
mergeMin defs x y =
  let x' = reifyEOP defs x
      y' = reifyEOP defs y
      z  = fst (runWriter (N.normaliseNat (mkTyConApp typeNatSubTyCon [y',x'])))
#if MIN_VERSION_ghc_typelits_natnormalise(0,7,0)
  in  case runWriterT (N.isNatural z) of
        Just (True, cs) | Set.null cs -> (x, Normalised)
        Just (False,cs) | Set.null cs -> (y, Normalised)
#else
  in  case N.isNatural z of
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

-- | Try to factor out terms in the logarithm. I.e., reduce `Log b (n * b^f)`
-- to `(Log b n) + f`.
tryFactorLog
  :: (ExtraOp -> ExtraOp -> ExtraOp)
  -> ExtraDefs
  -> ExtraOp
  -> ExtraOp
  -> Maybe NormaliseResult
tryFactorLog logConstr defs x y = result
  where
    -- Get SOP from Natnormalise plugins and check if the sum of products
    -- contains forall v. base^v in all products. If this is the case
    -- we can extract the v outside of the log and eliminate the base*v from
    -- the log. I.e., reduce `Log b (n * b^f)` to `(Log b n) + f`.
    --
    -- TODO: We could go even further and find the smallest common factor
    -- and extract it out. Probably only worth it if it is a literal
    mkProduct [] = N.P [N.I 1]
    mkProduct xs = N.P xs
    extractFactor _ _ [] = Nothing
    extractFactor ac b (c:cs)
      | b == (N.CType (N.reifySOP (N.S [N.P [c]])))
      = Just (mkProduct ((reverse ac) ++ cs), mkNumLitTy 1)
    extractFactor ac b (((N.E c v)):cs)
      | b == N.CType (N.reifySOP c)
      = Just (mkProduct ((reverse ac) ++ cs), N.reifySOP (N.S [v]))
    extractFactor ac b (c:cs) = extractFactor (c:ac) b cs
    allSame [] = True
    allSame (v:vs) = all (v ==) vs

    x1 = N.CType (reifyEOP defs x)
    (ySOP, ltCts) = runWriter (N.normaliseNat (reifyEOP defs y))
    resultM = do
      newProductsAndFactors <- sequence (fmap (extractFactor [] x1 . N.unP) (N.unS ySOP))
      let (newProducts, factors) = unzip newProductsAndFactors
      let factor = head factors
      newLogOf <- toExtraOp defs (N.reifySOP (N.S newProducts))
      let newLog = reifyEOP defs (logConstr x newLogOf)
      let normalisedTy = mkTyConApp typeNatAddTyCon [newLog, factor]
      if allSame (fmap N.CType factors) && Prelude.null ltCts
        then toExtraOp defs normalisedTy
        else Nothing

    result = case resultM of
              Just norm -> Just (norm, Normalised)
              Nothing   -> Just (logConstr x y, Untouched)

mergeFLog :: ExtraDefs -> ExtraOp -> ExtraOp -> Maybe NormaliseResult
mergeFLog _ (I i) _         | i < 2  = Nothing
mergeFLog _ i     (Exp j k) | i == j = Just (k, Normalised)
mergeFLog _ (I i) (I j)              = fmap (\r -> (I r, Normalised)) (flogBase i j)
mergeFLog defs x y                   = tryFactorLog FLog defs x y

mergeCLog :: ExtraDefs -> ExtraOp -> ExtraOp -> Maybe NormaliseResult
mergeCLog _ (I i) _         | i < 2  = Nothing
mergeCLog _ i     (Exp j k) | i == j = Just (k, Normalised)
mergeCLog _ (I i) (I j)              = fmap (\r -> (I r, Normalised)) (clogBase i j)
mergeCLog defs x y                   = tryFactorLog CLog defs x y

mergeLog :: ExtraDefs -> ExtraOp -> ExtraOp -> Maybe NormaliseResult
mergeLog _ (I i) _          | i < 2   = Nothing
mergeLog _ b     (Exp b' y) | b == b' = Just (y, Normalised)
mergeLog _ (I i) (I j)                = fmap (\r -> (I r, Normalised)) (exactLogBase i j)
mergeLog defs x y                     = tryFactorLog Log defs x y

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
