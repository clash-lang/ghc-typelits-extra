{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module GHC.TypeLits.Extra.Solver.Unify
  ( UnifyResult (..)
  , NormaliseResult
  , normaliseNat
  , unifyExtra
  )
where

-- external
import Control.Monad.Trans.Class    (lift)
import Control.Monad.Trans.Maybe    (MaybeT (..))
import Data.Maybe                   (catMaybes)
import Data.Function                (on)
import GHC.TypeLits.Normalise.Unify (CType (..))

-- ghc-tcplugin-api
import GHC.TcPlugin.API

-- GHC API
import GHC.Builtin.Types.Literals (typeNatExpTyCon)
import GHC.Core.TyCo.Rep (Type (..), TyLit (..))
import GHC.Core.Type (coreView)
import GHC.Types.Unique.Set (UniqSet, emptyUniqSet, unionUniqSets, unitUniqSet)
import GHC.Utils.Outputable (($$), text)

-- internal
import GHC.TypeLits.Extra.Solver.Compat
import GHC.TypeLits.Extra.Solver.Operations

mergeNormResWith
  :: (ExtraOp -> ExtraOp -> MaybeT (TcPluginM 'Solve) NormaliseResult)
  -> MaybeT (TcPluginM 'Solve) NormaliseResult
  -> MaybeT (TcPluginM 'Solve) NormaliseResult
  -> MaybeT (TcPluginM 'Solve) NormaliseResult
mergeNormResWith f x y = do
  (x', n1) <- x
  (y', n2) <- y
  (res, n3) <- f x' y'
  pure (res, n1 `mergeNormalised` n2 `mergeNormalised` n3)


normaliseNat :: ExtraDefs -> Type -> MaybeT (TcPluginM 'Solve) NormaliseResult
normaliseNat defs = go
 where
  go :: Type -> MaybeT (TcPluginM 'Solve) NormaliseResult
  go ty | Just ty1 <- coreView ty = go ty1
  go (TyVarTy v)          = pure (V v, Untouched)
  go (LitTy (NumTyLit i)) = pure (I i, Untouched)

  go (TyConApp tc [x,y])
    | tc == maxTyCon defs
    = mergeNormResWith (\x' y' -> return (mergeMax defs x' y'))
                       (go x)
                       (go y)
    | tc == minTyCon defs
    = mergeNormResWith (\x' y' -> return (mergeMin defs x' y'))
                       (go x)
                       (go y)
    | tc == divTyCon defs
    = mergeNormResWith (\x' y' -> MaybeT (return (mergeDiv x' y')))
                       (go x)
                       (go y)
    | tc == modTyCon defs
    = mergeNormResWith (\x' y' -> MaybeT (return (mergeMod x' y')))
                       (go x)
                       (go y)
    | tc == flogTyCon defs
    = mergeNormResWith (\x' y' -> MaybeT (return (mergeFLog x' y')))
                       (go x)
                       (go y)
    | tc == clogTyCon defs
    = mergeNormResWith (\x' y' -> MaybeT (return (mergeCLog x' y')))
                       (go x)
                       (go y)
    | tc == logTyCon defs
    = mergeNormResWith (\x' y' -> MaybeT (return (mergeLog x' y')))
                       (go x)
                       (go y)
    | tc == gcdTyCon defs
    = mergeNormResWith (\x' y' -> return (mergeGCD x' y'))
                       (go x)
                       (go y)
    | tc == lcmTyCon defs
    = mergeNormResWith (\x' y' -> return (mergeLCM x' y'))
                       (go x)
                       (go y)
    | tc == typeNatExpTyCon
    = mergeNormResWith (\x' y' -> return (mergeExp x' y'))
                       (go x)
                       (go y)

  go (TyConApp tc [x,y,z])
    | tc == clogWZTyCon defs
    = do (x', n1) <- normaliseNat defs x
         (y', n2) <- normaliseNat defs y
         (z', n3) <- normaliseNat defs z
         (res, n4) <- MaybeT $ return $ mergeCLogWZ x' y' z'
         pure (res, foldl mergeNormalised Untouched [n1,n2,n3,n4])

  go (TyConApp tc tys) = do
    let mergeExtraOp [] = []
        mergeExtraOp ((Just (op, Normalised []), _):xs) = reifyEOP defs op:mergeExtraOp xs
        mergeExtraOp ((_, ty):xs) = ty:mergeExtraOp xs

    normResults <- lift (sequence (runMaybeT . go <$> tys))
    let anyNormalised = foldr mergeNormalised Untouched (snd <$> catMaybes normResults)
    let tys' = mergeExtraOp (zip normResults tys)
    pure (C (CType (TyConApp tc tys')), anyNormalised)

  go t = return (C (CType t), Untouched)

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

unifyExtra :: Ct -> ExtraOp -> ExtraOp -> TcPluginM 'Solve UnifyResult
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
fvOP (I _)          = emptyUniqSet
fvOP (V v)          = unitUniqSet v
fvOP (C _)          = emptyUniqSet
fvOP (Max x y)      = fvOP x `unionUniqSets` fvOP y
fvOP (Min x y)      = fvOP x `unionUniqSets` fvOP y
fvOP (Div x y)      = fvOP x `unionUniqSets` fvOP y
fvOP (Mod x y)      = fvOP x `unionUniqSets` fvOP y
fvOP (FLog x y)     = fvOP x `unionUniqSets` fvOP y
fvOP (CLog x y)     = fvOP x `unionUniqSets` fvOP y
fvOP (CLogWZ x y z) = fvOP x `unionUniqSets` fvOP y `unionUniqSets` fvOP z
fvOP (Log x y)      = fvOP x `unionUniqSets` fvOP y
fvOP (GCD x y)      = fvOP x `unionUniqSets` fvOP y
fvOP (LCM x y)      = fvOP x `unionUniqSets` fvOP y
fvOP (Exp x y)      = fvOP x `unionUniqSets` fvOP y

eqFV :: ExtraOp -> ExtraOp -> Bool
eqFV = (==) `on` fvOP

containsConstants :: ExtraOp -> Bool
containsConstants = \case
  I _          -> False
  V _          -> False
  C _          -> True
  Max x y      -> or $ containsConstants <$> [x, y]
  Min x y      -> or $ containsConstants <$> [x, y]
  Div x y      -> or $ containsConstants <$> [x, y]
  Mod x y      -> or $ containsConstants <$> [x, y]
  FLog x y     -> or $ containsConstants <$> [x, y]
  CLog x y     -> or $ containsConstants <$> [x, y]
  CLogWZ x y z -> or $ containsConstants <$> [x, y, z]
  Log x y      -> or $ containsConstants <$> [x, y]
  GCD x y      -> or $ containsConstants <$> [x, y]
  LCM x y      -> or $ containsConstants <$> [x, y]
  Exp x y      -> or $ containsConstants <$> [x, y]
