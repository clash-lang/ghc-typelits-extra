{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}

module GHC.TypeLits.Extra.Solver.Unify
  ( ExtraDefs (..)
  , UnifyResult (..)
  , NormaliseResult
  , normaliseNat
  , toExtraOp
  , unifyExtra
  )
where

-- external
import Data.Maybe                   (catMaybes)
import Data.Function                (on)
import GHC.TypeLits.Normalise.Unify (CType (..))

-- GHC API
#if MIN_VERSION_ghc(9,0,0)
import GHC.Builtin.Types.Literals (typeNatExpTyCon)
import GHC.Core.TyCo.Rep (Type (..), TyLit (..))
import GHC.Core.Type (TyVar, coreView)
import GHC.Tc.Plugin (TcPluginM, tcPluginTrace)
import GHC.Tc.Types.Constraint (Ct)
import GHC.Types.Unique.Set (UniqSet, emptyUniqSet, unionUniqSets, unitUniqSet)
import GHC.Utils.Outputable (Outputable (..), ($$), text)
#else
import Outputable (Outputable (..), ($$), text)
import TcPluginM  (TcPluginM, tcPluginTrace)
import TcTypeNats (typeNatExpTyCon)
import Type       (TyVar, coreView)
import TyCoRep    (Type (..), TyLit (..))
import UniqSet    (UniqSet, emptyUniqSet, unionUniqSets, unitUniqSet)
#if MIN_VERSION_ghc(8,10,0)
import Constraint (Ct)
#else
import TcRnMonad  (Ct)
#endif
#endif

-- internal
import GHC.TypeLits.Extra.Solver.Operations

mergeNormResWith
  :: (ExtraOp -> ExtraOp -> Maybe NormaliseResult)
  -> Maybe NormaliseResult
  -> Maybe NormaliseResult
  -> Maybe NormaliseResult
mergeNormResWith f x y = do
  (x', n1) <- x
  (y', n2) <- y
  (res, n3) <- f x' y'
  pure (res, n1 `mergeNormalised` n2 `mergeNormalised` n3)

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
toExtraOp _ t = Just (C (CType t))

normaliseNat :: ExtraDefs -> ExtraOp -> Maybe NormaliseResult
normaliseNat _ (I i) = pure (I i, Untouched)
normaliseNat _ (V v) = pure (V v, Untouched)
normaliseNat defs (Max x y) = mergeNormResWith (\x' y' -> pure (mergeMax defs x' y'))
                                (normaliseNat defs x)
                                (normaliseNat defs y)
normaliseNat defs (Min x y) = mergeNormResWith (\x' y' -> pure (mergeMin defs x' y'))
                                (normaliseNat defs x)
                                (normaliseNat defs y)
normaliseNat defs (Div x y) = mergeNormResWith (\x' y' -> (mergeDiv x' y'))
                                (normaliseNat defs x)
                                (normaliseNat defs y)
normaliseNat defs (Mod x y) = mergeNormResWith (\x' y' -> (mergeMod x' y'))
                                (normaliseNat defs x)
                                (normaliseNat defs y)
normaliseNat defs (FLog x y) = mergeNormResWith (\x' y' -> (mergeFLog x' y'))
                                (normaliseNat defs x)
                                (normaliseNat defs y)
normaliseNat defs (CLog x y) = mergeNormResWith (\x' y' -> (mergeCLog x' y'))
                                (normaliseNat defs x)
                                (normaliseNat defs y)
normaliseNat defs (Log x y) = mergeNormResWith (\x' y' -> (mergeLog x' y'))
                               (normaliseNat defs x)
                               (normaliseNat defs y)
normaliseNat defs (GCD x y) = mergeNormResWith (\x' y' -> pure (mergeGCD x' y'))
                               (normaliseNat defs x)
                               (normaliseNat defs y)
normaliseNat defs (LCM x y) = mergeNormResWith (\x' y' -> pure (mergeLCM x' y'))
                               (normaliseNat defs x)
                               (normaliseNat defs y)
normaliseNat defs (Exp x y) = mergeNormResWith (\x' y' -> pure (mergeExp x' y'))
                               (normaliseNat defs x)
                               (normaliseNat defs y)
normaliseNat defs (C (CType (TyConApp tc tys))) = do
  let mergeExtraOp [] = []
      mergeExtraOp ((Just (op, Normalised), _):xs) = reifyEOP defs op:mergeExtraOp xs
      mergeExtraOp ((_, ty):xs) = ty:mergeExtraOp xs

  let normResults = map (\t -> toExtraOp defs t >>= normaliseNat defs) tys
  let anyNormalised = foldr mergeNormalised Untouched (snd <$> catMaybes normResults)
  let tys' = mergeExtraOp (zip normResults tys)
  pure (C (CType (TyConApp tc tys')), anyNormalised)

normaliseNat _ eop = Just (eop, Untouched)

-- | Result of comparing two 'SOP' terms, returning a potential substitution
-- list under which the two terms are equal.
data UnifyResult
  = Win  -- ^ Two terms are equal
  | Lose -- ^ Two terms are /not/ equal
  | Draw -- ^ We don't know if the two terms are equal

instance Outputable UnifyResult where
  ppr Win  = text "Win"
  ppr Lose = text "Lose"
  ppr Draw = text "Draw"

unifyExtra :: Ct -> ExtraOp -> ExtraOp -> TcPluginM UnifyResult
unifyExtra ct u v = do
  tcPluginTrace "unifyExtra" (ppr ct $$ ppr u $$ ppr v)
  return (unifyExtra' u v)

unifyExtra' :: ExtraOp -> ExtraOp -> UnifyResult
unifyExtra' u v
  | eqFV u v
  = go u v
  | otherwise
  = Draw
  where
    go a b | a == b = Win
    -- The following operations commute
    go (Max a b) (Max x y) = commuteResult (go a y) (go b x)
    go (Min a b) (Min x y) = commuteResult (go a y) (go b x)
    go (GCD a b) (GCD x y) = commuteResult (go a y) (go b x)
    go (LCM a b) (LCM x y) = commuteResult (go a y) (go b x)
    -- If there are operations contained in the type which this solver does
    -- not understand, then the result is a Draw
    go a b = if containsConstants a || containsConstants b then Draw else Lose

    commuteResult Win  Win  = Win
    commuteResult Lose _    = Lose
    commuteResult _    Lose = Lose
    commuteResult _    _    = Draw

fvOP :: ExtraOp -> UniqSet TyVar
fvOP (I _)      = emptyUniqSet
fvOP (V v)      = unitUniqSet v
fvOP (C _)      = emptyUniqSet
fvOP (Max x y)  = fvOP x `unionUniqSets` fvOP y
fvOP (Min x y)  = fvOP x `unionUniqSets` fvOP y
fvOP (Div x y)  = fvOP x `unionUniqSets` fvOP y
fvOP (Mod x y)  = fvOP x `unionUniqSets` fvOP y
fvOP (FLog x y) = fvOP x `unionUniqSets` fvOP y
fvOP (CLog x y) = fvOP x `unionUniqSets` fvOP y
fvOP (Log x y)  = fvOP x `unionUniqSets` fvOP y
fvOP (GCD x y)  = fvOP x `unionUniqSets` fvOP y
fvOP (LCM x y)  = fvOP x `unionUniqSets` fvOP y
fvOP (Exp x y)  = fvOP x `unionUniqSets` fvOP y

eqFV :: ExtraOp -> ExtraOp -> Bool
eqFV = (==) `on` fvOP

containsConstants :: ExtraOp -> Bool
containsConstants (I _) = False
containsConstants (V _) = False
containsConstants (C _) = True
containsConstants (Max x y)  = containsConstants x || containsConstants y
containsConstants (Min x y)  = containsConstants x || containsConstants y
containsConstants (Div x y)  = containsConstants x || containsConstants y
containsConstants (Mod x y)  = containsConstants x || containsConstants y
containsConstants (FLog x y) = containsConstants x || containsConstants y
containsConstants (CLog x y) = containsConstants x || containsConstants y
containsConstants (Log x y)  = containsConstants x || containsConstants y
containsConstants (GCD x y)  = containsConstants x || containsConstants y
containsConstants (LCM x y)  = containsConstants x || containsConstants y
containsConstants (Exp x y)  = containsConstants x || containsConstants y
