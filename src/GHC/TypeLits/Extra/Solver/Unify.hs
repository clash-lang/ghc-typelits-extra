module GHC.TypeLits.Extra.Solver.Unify where

-- external
import Data.Function (on)

-- GHC API
import Outputable (Outputable (..), (<+>), ($$), text)
import TcPluginM  (TcPluginM, tcPluginTrace)
import TcRnMonad  (Ct)
import Type       (TyVar, tcView, mkNumLitTy, mkTyConApp, mkTyVarTy)
import TyCon      (TyCon)
import TypeRep    (Type (..), TyLit (..))
import UniqSet    (UniqSet, emptyUniqSet, unionUniqSets, unitUniqSet)

-- internal
import GHC.TypeLits.Extra.Solver.Operations

data ExtraDefs = ExtraDefs
  { gcdTyCon :: TyCon
  }

normaliseNat :: ExtraDefs -> Type -> Maybe ExtraOp
normaliseNat defs ty | Just ty1 <- tcView ty = normaliseNat defs ty1
normaliseNat _ (TyVarTy v)          = pure (V v)
normaliseNat _ (LitTy (NumTyLit i)) = pure (I i)
normaliseNat defs (TyConApp tc [x,y])
  | tc == gcdTyCon defs = mergeGCD <$> normaliseNat defs x
                                   <*> normaliseNat defs y
  | otherwise           = Nothing
normaliseNat _ _ = Nothing

reifyExtraOp :: ExtraDefs -> ExtraOp -> Type
reifyExtraOp _    (I i)     = mkNumLitTy i
reifyExtraOp _    (V v)     = mkTyVarTy v
reifyExtraOp defs (GCD x y) = mkTyConApp (gcdTyCon defs)
                                         [reifyExtraOp defs x
                                         ,reifyExtraOp defs y
                                         ]

type ExtraSubst = [SubstItem]

data SubstItem
  = SubstItem
  { siVar  :: TyVar
  , siOP   :: ExtraOp
  , siNote :: Ct
  }

instance Outputable SubstItem where
  ppr si = ppr (siVar si) <+> text " := " <+> ppr (siOP si)

substsExtra :: ExtraSubst -> ExtraOp -> ExtraOp
substsExtra []     u = u
substsExtra (si:s) u = substsExtra s (substExtra (siVar si) (siOP si) u)

substExtra :: TyVar -> ExtraOp -> ExtraOp -> ExtraOp
substExtra _  _ i@(I _)   = i
substExtra tv e v@(V tv')
  | tv == tv'             = e
  | otherwise             = v
substExtra tv e (GCD x y) = mergeGCD (substExtra tv e x) (substExtra tv e y)

substsSubst :: ExtraSubst -> ExtraSubst -> ExtraSubst
substsSubst s = map (\si -> si {siOP = substsExtra s (siOP si)})

-- | Result of comparing two 'SOP' terms, returning a potential substitution
-- list under which the two terms are equal.
data UnifyResult
  = Win             -- ^ Two terms are equal
  | Lose            -- ^ Two terms are /not/ equal
  | Draw ExtraSubst -- ^ Two terms are only equal if the given substitution holds

instance Outputable UnifyResult where
  ppr Win          = text "Win"
  ppr (Draw subst) = text "Draw" <+> ppr subst
  ppr Lose         = text "Lose"

unifyExtra :: Ct -> ExtraOp -> ExtraOp -> TcPluginM UnifyResult
unifyExtra ct u v = do
  tcPluginTrace "unifyExtra" (ppr ct $$ ppr u $$ ppr v)
  return (unifyExtra' ct u v)

unifyExtra' :: Ct -> ExtraOp -> ExtraOp -> UnifyResult
unifyExtra' _ u v
  | eqFV u v  = if u == v then Win else Lose
  | otherwise = Draw []

fvOP :: ExtraOp -> UniqSet TyVar
fvOP (I _)     = emptyUniqSet
fvOP (V v)     = unitUniqSet v
fvOP (GCD x y) = fvOP x `unionUniqSets` fvOP y

eqFV :: ExtraOp -> ExtraOp -> Bool
eqFV = (==) `on` fvOP
