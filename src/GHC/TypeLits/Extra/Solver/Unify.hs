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
  , unifyExtra
  )
where

-- external
import Control.Monad.Trans.Class    (lift)
import Control.Monad.Trans.Maybe    (MaybeT (..))
import Control.Monad.Trans.Writer.Strict (runWriter)
import Data.Maybe                   (catMaybes)
import Data.Function                (on)
import GHC.TypeLits.Normalise.Unify (CType (..))
import qualified GHC.TypeLits.Normalise.Unify as Normalise

-- GHC API
#if MIN_VERSION_ghc(9,0,0)
import GHC.Builtin.Types.Literals
  (typeNatExpTyCon, typeNatMulTyCon, typeNatAddTyCon, typeNatSubTyCon)
import GHC.Core.TyCo.Rep (Type (..), TyLit (..))
import GHC.Core.TyCon (TyCon)
import GHC.Core.Type (TyVar, coreView)
import GHC.Tc.Plugin (TcPluginM, tcPluginTrace)
import GHC.Tc.Types.Constraint (Ct)
import GHC.Types.Unique.Set (UniqSet, emptyUniqSet, unionUniqSets, unitUniqSet)
import GHC.Utils.Outputable (Outputable (..), ($$), text)
#else
import Outputable (Outputable (..), ($$), text)
import TcPluginM  (TcPluginM, tcPluginTrace)
import TcTypeNats (typeNatExpTyCon, typeNatMulTyCon, typeNatAddTyCon, typeNatSubTyCon)
import Type       (TyVar, coreView)
import TyCon (TyCon)
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
  :: (ExtraOp -> ExtraOp -> MaybeT TcPluginM NormaliseResult)
  -> MaybeT TcPluginM NormaliseResult
  -> MaybeT TcPluginM NormaliseResult
  -> MaybeT TcPluginM NormaliseResult
mergeNormResWith f x y = do
  (x', n1) <- x
  (y', n2) <- y
  (res, n3) <- f x' y'
  pure (res, n1 `mergeNormalised` n2 `mergeNormalised` n3)

-- | First normalise the expression using the solver from ghc-typelits-natnormalise
-- before calling the mergeWith operation from this ghc-typelits-extra solver.
withSOPNormalize ::
  ExtraDefs ->
  -- The mergeWith operation
  (ExtraOp -> ExtraOp -> (ExtraOp, Normalised)) ->
  -- The (+), (-), or (*) type constructor
  TyCon ->
  Type ->
  Type ->
  MaybeT TcPluginM NormaliseResult
withSOPNormalize defs mergeWith tc x y = do
  (x1, n1) <- normaliseNat defs x
  (y1, n2) <- normaliseNat defs y
  let x2 = reifyEOP defs x1
      y2 = reifyEOP defs y1
      (q,subtractions) = runWriter (Normalise.normaliseNat (TyConApp tc [x2, y2]))
      -- Currently we don't use the result of ghc-typelits-natnormalise when there
      -- are "inner" subtractions, as we would have to emit inequality constraints
      -- for the arguments of those subtractions to guarantee that the result of
      -- those subtractions would result in a natural number.
      --
      -- TODO: emit inequality constraints for the inner subtractions
      noSubtractions
        | tc == typeNatSubTyCon
        , [(s1, s2)] <- subtractions
        = CType s1 == CType x2 && CType s2 == CType y2
        | otherwise
        = null subtractions
  if noSubtractions then case Normalise.reifySOP q of
    TyConApp tcQ [xQ,yQ]
      | tcQ == tc -> do
        (x3, n3) <- normaliseNat defs xQ
        (y3, n4) <- normaliseNat defs yQ
        let (res,n5) = mergeWith x3 y3
        return (res, mconcat [n1,n2,n3,n4,n5])
    resSOP -> do
      (res, n3) <- normaliseNat defs resSOP
      return (res, mconcat [n1,n2,n3])
  else do
    let (res, n3) = mergeWith x1 y1
    return (res, mconcat [n1,n2,n3])

normaliseNat :: ExtraDefs -> Type -> MaybeT TcPluginM NormaliseResult
normaliseNat defs ty | Just ty1 <- coreView ty = normaliseNat defs ty1
normaliseNat _ (TyVarTy v)          = pure (V v, Untouched)
normaliseNat _ (LitTy (NumTyLit i)) = pure (I i, Untouched)
normaliseNat defs (TyConApp tc [x,y])
  | tc == typeNatAddTyCon = withSOPNormalize defs mergePlus tc x y
  | tc == typeNatSubTyCon = withSOPNormalize defs mergeSub tc x y
  | tc == typeNatMulTyCon = withSOPNormalize defs mergeMul tc x y
  | tc == maxTyCon defs = mergeNormResWith (\x' y' -> return (mergeMax defs x' y'))
                                           (normaliseNat defs x)
                                           (normaliseNat defs y)
  | tc == minTyCon defs = mergeNormResWith (\x' y' -> return (mergeMin defs x' y'))
                                           (normaliseNat defs x)
                                           (normaliseNat defs y)
  | tc == divTyCon defs = mergeNormResWith (\x' y' -> MaybeT (return (mergeDiv x' y')))
                                           (normaliseNat defs x)
                                           (normaliseNat defs y)
  | tc == modTyCon defs = mergeNormResWith (\x' y' -> MaybeT (return (mergeMod x' y')))
                                           (normaliseNat defs x)
                                           (normaliseNat defs y)
  | tc == flogTyCon defs = mergeNormResWith (\x' y' -> MaybeT (return (mergeFLog x' y')))
                                           (normaliseNat defs x)
                                           (normaliseNat defs y)
  | tc == clogTyCon defs = mergeNormResWith (\x' y' -> MaybeT (return (mergeCLog x' y')))
                                           (normaliseNat defs x)
                                           (normaliseNat defs y)
  | tc == logTyCon defs = mergeNormResWith (\x' y' -> MaybeT (return (mergeLog x' y')))
                                           (normaliseNat defs x)
                                           (normaliseNat defs y)
  | tc == gcdTyCon defs = mergeNormResWith (\x' y' -> return (mergeGCD x' y'))
                                           (normaliseNat defs x)
                                           (normaliseNat defs y)
  | tc == lcmTyCon defs = mergeNormResWith (\x' y' -> return (mergeLCM x' y'))
                                           (normaliseNat defs x)
                                           (normaliseNat defs y)
  | tc == typeNatExpTyCon = mergeNormResWith (\x' y' -> return (mergeExp x' y'))
                                             (normaliseNat defs x)
                                             (normaliseNat defs y)

normaliseNat defs (TyConApp tc tys) = do
  let mergeExtraOp [] = []
      mergeExtraOp ((Just (op, Normalised), _):xs) = reifyEOP defs op:mergeExtraOp xs
      mergeExtraOp ((_, ty):xs) = ty:mergeExtraOp xs

  normResults <- lift (sequence (runMaybeT . normaliseNat defs <$> tys))
  let anyNormalised = foldr mergeNormalised Untouched (snd <$> catMaybes normResults)
  let tys' = mergeExtraOp (zip normResults tys)
  pure (C (CType (TyConApp tc tys')), anyNormalised)

normaliseNat _ t = return (C (CType t), Untouched)

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
fvOP (Plus x y) = fvOP x `unionUniqSets` fvOP y
fvOP (Sub x y)  = fvOP x `unionUniqSets` fvOP y
fvOP (Mul  x y) = fvOP x `unionUniqSets` fvOP y
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
containsConstants (Plus _ _) = True
containsConstants (Sub _ _) = True
containsConstants (Mul _ _) = True
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
