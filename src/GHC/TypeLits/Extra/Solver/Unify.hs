{-|
Copyright  :  (C) 2015-2016, University of Twente
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
import Data.Function (on)

-- GHC API
import Outputable (Outputable (..), ($$), text)
import TcPluginM  (TcPluginM, tcPluginTrace)
import TcRnMonad  (Ct)
import TcTypeNats (typeNatAddTyCon, typeNatExpTyCon, typeNatMulTyCon,
                   typeNatSubTyCon)
import Type       (TyVar, coreView)
import TyCon      (TyCon)
#if __GLASGOW_HASKELL__ >= 711
import TyCoRep    (Type (..), TyLit (..))
#else
import TypeRep    (Type (..), TyLit (..))
#endif
import UniqSet    (UniqSet, emptyUniqSet, unionUniqSets, unitUniqSet)

-- internal
import GHC.TypeLits.Extra.Solver.Operations

data ExtraDefs = ExtraDefs
  { gcdTyCon  :: TyCon
  , clogTyCon :: TyCon
  }

normaliseNat :: ExtraDefs -> Type -> Maybe ExtraOp
normaliseNat defs ty | Just ty1 <- coreView ty = normaliseNat defs ty1
normaliseNat _ (TyVarTy v)          = pure (V v)
normaliseNat _ (LitTy (NumTyLit i)) = pure (I i)
normaliseNat defs (TyConApp tc [x,y])
  | tc == gcdTyCon defs = mergeGCD <$> normaliseNat defs x
                                   <*> normaliseNat defs y
  | tc == clogTyCon defs = do x' <- normaliseNat defs x
                              y' <- normaliseNat defs y
                              mergeCLog x' y'
  | tc == typeNatExpTyCon = mergeExp <$> normaliseNat defs x
                                     <*> normaliseNat defs y
  | tc == typeNatAddTyCon = do x' <- normaliseNat defs x
                               y' <- normaliseNat defs y
                               mergeAdd x' y'
  | tc == typeNatSubTyCon = do x' <- normaliseNat defs x
                               y' <- normaliseNat defs y
                               mergeSub x' y'
  | tc == typeNatMulTyCon = do x' <- normaliseNat defs x
                               y' <- normaliseNat defs y
                               mergeMul x' y'
  | otherwise = Nothing
normaliseNat _ _ = Nothing

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
  | eqFV u v  = if u == v then Win else Lose
  | otherwise = Draw

fvOP :: ExtraOp -> UniqSet TyVar
fvOP (I _)      = emptyUniqSet
fvOP (V v)      = unitUniqSet v
fvOP (GCD x y)  = fvOP x `unionUniqSets` fvOP y
fvOP (CLog x y) = fvOP x `unionUniqSets` fvOP y
fvOP (Exp x y)  = fvOP x `unionUniqSets` fvOP y

eqFV :: ExtraOp -> ExtraOp -> Bool
eqFV = (==) `on` fvOP
