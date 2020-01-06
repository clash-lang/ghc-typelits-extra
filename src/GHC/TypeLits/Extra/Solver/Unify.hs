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
  , normaliseNat
  , unifyExtra
  )
where

-- external
import Control.Monad.Trans.Class    (lift)
import Control.Monad.Trans.Maybe    (MaybeT (..))
import Control.Monad.Trans.Writer.Strict (runWriter)
import Data.Function                (on)
import GHC.TypeLits.Normalise.Unify (CType (..))
import qualified GHC.TypeLits.Normalise.Unify as Normalise

-- GHC API
import Outputable (Outputable (..), ($$), text)
import TcPluginM  (TcPluginM, matchFam, tcPluginTrace)
import TcTypeNats (typeNatExpTyCon)
import Type       (TyVar, coreView)
import TyCoRep    (Type (..), TyLit (..))
import UniqSet    (UniqSet, emptyUniqSet, unionUniqSets, unitUniqSet)

#if MIN_VERSION_ghc(8,10,0)
import Constraint (Ct)
#else
import TcRnMonad  (Ct)
#endif

-- internal
import GHC.TypeLits.Extra.Solver.Operations

normaliseNat :: ExtraDefs -> Type -> MaybeT TcPluginM ExtraOp
normaliseNat defs ty | Just ty1 <- coreView ty = normaliseNat defs ty1
normaliseNat _ (TyVarTy v)          = pure (V v)
normaliseNat _ (LitTy (NumTyLit i)) = pure (I i)
normaliseNat defs (TyConApp tc [x,y])
  | tc == maxTyCon defs = mergeMax defs <$> normaliseNat defs x
                                        <*> normaliseNat defs y
  | tc == minTyCon defs = mergeMin defs <$> normaliseNat defs x
                                        <*> normaliseNat defs y
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
    _ -> let q = fst (runWriter (Normalise.normaliseNat (TyConApp tc tys)))
         in  return (C (CType (Normalise.reifySOP q)))

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
