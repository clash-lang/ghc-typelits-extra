{-|
Copyright  :  (C) 2015-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

module GHC.TypeLits.Extra.Solver.Unify
  ( ExtraDefs (..)
  , UnifyResult (..)
  , normaliseNat
  , unifyExtra
  )
where

-- external
import Control.Monad.Trans.Class    (lift)
import Control.Monad.Trans.Maybe    (MaybeT (..))
import Data.Function                (on)
import GHC.TypeLits.Normalise.Unify (CType (..))

-- GHC API
import Outputable (Outputable (..), ($$), text)
import TcPluginM  (TcPluginM, matchFam, tcPluginTrace)
import TcRnMonad  (Ct)
import TcTypeNats (typeNatExpTyCon)
import Type       (TyVar, coreView, mkNumLitTy, mkTyConApp, mkTyVarTy)
import TyCon      (TyCon)
import TyCoRep    (Type (..), TyLit (..))
import UniqSet    (UniqSet, emptyUniqSet, unionUniqSets, unitUniqSet)

-- internal
import GHC.TypeLits.Extra.Solver.Operations

data ExtraDefs = ExtraDefs
  { divTyCon  :: TyCon
  , modTyCon  :: TyCon
  , flogTyCon :: TyCon
  , clogTyCon :: TyCon
  , logTyCon  :: TyCon
  , gcdTyCon  :: TyCon
  , lcmTyCon  :: TyCon
  }

normaliseNat :: ExtraDefs -> Type -> MaybeT TcPluginM ExtraOp
normaliseNat defs ty | Just ty1 <- coreView ty = normaliseNat defs ty1
normaliseNat _ (TyVarTy v)          = pure (V v)
normaliseNat _ (LitTy (NumTyLit i)) = pure (I i)
normaliseNat defs (TyConApp tc [x,y])
  | tc == divTyCon defs = do x' <- normaliseNat defs x
                             y' <- normaliseNat defs y
                             MaybeT (return (mergeDiv x' y'))
  | tc == modTyCon defs = do x' <- normaliseNat defs x
                             y' <- normaliseNat defs y
                             MaybeT (return (mergeMod x' y'))
  | tc == flogTyCon defs = do x' <- normaliseNat defs x
                              y' <- normaliseNat defs y
                              MaybeT (return (mergeFLog x' y'))
  | tc == clogTyCon defs = do x' <- normaliseNat defs x
                              y' <- normaliseNat defs y
                              MaybeT (return (mergeCLog x' y'))
  | tc == logTyCon defs = do x' <- normaliseNat defs x
                             y' <- normaliseNat defs y
                             MaybeT (return (mergeLog x' y'))
  | tc == gcdTyCon defs = mergeGCD <$> normaliseNat defs x
                                   <*> normaliseNat defs y
  | tc == lcmTyCon defs = mergeLCM <$> normaliseNat defs x
                                   <*> normaliseNat defs y
  | tc == typeNatExpTyCon = mergeExp <$> normaliseNat defs x
                                     <*> normaliseNat defs y

normaliseNat defs (TyConApp tc tys) = do
  tys'   <- mapM (fmap (reifyEOP defs) . normaliseNat defs) tys
  tyM    <- lift (matchFam tc tys')
  case tyM of
    Just (_,ty) -> normaliseNat defs ty
    _ -> return (C (CType (TyConApp tc tys)))

normaliseNat _ t = return (C (CType t))

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
  return (unifyExtra' ct u v)

unifyExtra' :: Ct -> ExtraOp -> ExtraOp -> UnifyResult
unifyExtra' _ u v
  | eqFV u v  = if u == v then Win
                          else if containsConstants u || containsConstants v
                                  then Draw
                                  else Lose
  | otherwise = Draw

fvOP :: ExtraOp -> UniqSet TyVar
fvOP (I _)      = emptyUniqSet
fvOP (V v)      = unitUniqSet v
fvOP (C _)      = emptyUniqSet
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

reifyEOP :: ExtraDefs -> ExtraOp -> Type
reifyEOP _ (I i) = mkNumLitTy i
reifyEOP _ (V v) = mkTyVarTy v
reifyEOP _ (C (CType c)) = c
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


containsConstants :: ExtraOp -> Bool
containsConstants (I _) = False
containsConstants (V _) = False
containsConstants (C _) = True
containsConstants (Div x y)  = containsConstants x || containsConstants y
containsConstants (Mod x y)  = containsConstants x || containsConstants y
containsConstants (FLog x y) = containsConstants x || containsConstants y
containsConstants (CLog x y) = containsConstants x || containsConstants y
containsConstants (Log x y)  = containsConstants x || containsConstants y
containsConstants (GCD x y)  = containsConstants x || containsConstants y
containsConstants (LCM x y)  = containsConstants x || containsConstants y
containsConstants (Exp x y)  = containsConstants x || containsConstants y
