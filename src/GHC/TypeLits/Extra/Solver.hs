{-|
Copyright  :  (C) 2015-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

To use the plugin, add the

@
{\-\# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver \#-\}
@

pragma to the header of your file

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module GHC.TypeLits.Extra.Solver
  ( plugin )
where

-- external
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Maybe (catMaybes)

-- ghc-tcplugin-api
import GHC.TcPlugin.API
import GHC.TcPlugin.API.TyConSubst

-- GHC API
import GHC.Builtin.Types (promotedTrueDataCon, promotedFalseDataCon)
import GHC.Core.DataCon (dataConWrapId)
import GHC.Core.TyCo.Rep (Type (..), TyLit (..))
import GHC.Driver.Plugins (Plugin (..), defaultPlugin, purePlugin)
import GHC.Tc.Types.Constraint (isWantedCt)
import GHC.Utils.Outputable ((<+>), ($$), text, vcat)

-- ghc-typelits-natnormalise
import GHC.TypeLits.Normalise.Compat

-- internal
import GHC.TypeLits.Extra.Solver.Compat
import GHC.TypeLits.Extra.Solver.Operations
import GHC.TypeLits.Extra.Solver.Unify

-- | A solver implement as a type-checker plugin for:
--
--     * 'Div': type-level 'div'
--
--     * 'Mod': type-level 'mod'
--
--     * 'FLog': type-level equivalent of <https://hackage.haskell.org/package/base-4.17.0.0/docs/GHC-Integer-Logarithms.html#v:integerLogBase-35- integerLogBase#>
--       .i.e. the exact integer equivalent to "@'floor' ('logBase' x y)@"
--
--     * 'CLog': type-level equivalent of /the ceiling of/ <https://hackage.haskell.org/package/base-4.17.0.0/docs/GHC-Integer-Logarithms.html#v:integerLogBase-35- integerLogBase#>
--       .i.e. the exact integer equivalent to "@'ceiling' ('logBase' x y)@"
--
--     * 'CLogWZ': extension of @CLog@, which returns the additional third argument in case the second argument is zero
--
--     * 'Log': type-level equivalent of <https://hackage.haskell.org/package/base-4.17.0.0/docs/GHC-Integer-Logarithms.html#v:integerLogBase-35- integerLogBase#>
--        where the operation only reduces when "@'floor' ('logBase' b x) ~ 'ceiling' ('logBase' b x)@"
--
--     * 'GCD': a type-level 'gcd'
--
--     * 'LCM': a type-level 'lcm'
--
-- To use the plugin, add
--
-- @
-- {\-\# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver \#-\}
-- @
--
-- To the header of your file.
plugin :: Plugin
plugin
  = defaultPlugin
  { tcPlugin = const (pure (mkTcPlugin normalisePlugin))
  , pluginRecompile = purePlugin
  }

normalisePlugin :: TcPlugin
normalisePlugin =
  TcPlugin { tcPluginInit    = lookupExtraDefs
           , tcPluginSolve   = decideEqualSOP
           , tcPluginRewrite = extraRewrite
           , tcPluginStop    = const (return ())
           }

extraRewrite :: ExtraDefs -> UniqFM TyCon TcPluginRewriter
extraRewrite defs = listToUFM
  [ (minTyCon defs, minRewrite)
  , (maxTyCon defs, maxRewrite)
  , (flogTyCon defs, flogRewrite)
  , (clogTyCon defs, clogRewrite)
  , (clogWZTyCon defs, clogWZRewrite)
  , (logTyCon defs, logRewrite)
  , (gcdTyCon defs, gcdRewrite)
  , (lcmTyCon defs, lcmRewrite)
  ]
  where
    minRewrite _ args
      | [LitTy (NumTyLit i), LitTy (NumTyLit j)] <- args
      = pure $ rewriteTo (minTyCon defs) args $ min i j
    minRewrite _ _ = pure TcPluginNoRewrite

    maxRewrite _ args
      | [LitTy (NumTyLit i), LitTy (NumTyLit j)] <- args
      = pure $ rewriteTo (maxTyCon defs) args $ max i j
    maxRewrite _ _ = pure TcPluginNoRewrite

    flogRewrite _ args
      | [LitTy (NumTyLit i), LitTy (NumTyLit j)] <- args
      , i > 1
      , Just r <- flogBase i j
      = pure $ rewriteTo (flogTyCon defs) args r
    flogRewrite _ _ = pure TcPluginNoRewrite

    clogRewrite _ args
      | [LitTy (NumTyLit i), LitTy (NumTyLit j)] <- args
      , i > 1
      , Just r <- clogBase i j
      = pure $ rewriteTo (clogTyCon defs) args r
    clogRewrite _ _ = pure TcPluginNoRewrite

    clogWZRewrite _ args
      | [_, LitTy (NumTyLit 0), z] <- args
      = pure $ TcPluginRewriteTo (reduce (clogWZTyCon defs) args z) []
    clogWZRewrite _ args
      | [LitTy (NumTyLit i), LitTy (NumTyLit j), _] <- args
      , i > 1
      , Just r <- clogBase i j
      = pure $ rewriteTo (clogWZTyCon defs) args r
    clogWZRewrite _ _ = pure TcPluginNoRewrite

    logRewrite _ args
      | [LitTy (NumTyLit i), LitTy (NumTyLit j)] <- args
      , i > 1
      , Just r <- exactLogBase i j
      = pure $ rewriteTo (logTyCon defs) args r
    logRewrite _ _ = pure TcPluginNoRewrite

    gcdRewrite _ args
      | [LitTy (NumTyLit i), LitTy (NumTyLit j)] <- args
      = pure $ rewriteTo (gcdTyCon defs) args (i `gcd` j)
    gcdRewrite _ _ = pure TcPluginNoRewrite

    lcmRewrite _ args
      | [LitTy (NumTyLit i), LitTy (NumTyLit j)] <- args
      = pure $ rewriteTo (lcmTyCon defs) args (i `lcm` j)
    lcmRewrite _ _ = pure TcPluginNoRewrite

    rewriteTo tyCon args x =
      TcPluginRewriteTo (reduce tyCon args (LitTy (NumTyLit x))) []

    reduce tc args res = Reduction co res
     where
      co = mkPluginUnivCo "ghc-typelits-extra" Nominal []
             (mkTyConApp tc args) res


decideEqualSOP :: ExtraDefs -> [Ct] -> [Ct] -> TcPluginM 'Solve TcPluginSolveResult
decideEqualSOP _    _givens []      = return (TcPluginOk [] [])
decideEqualSOP defs givens  wanteds = do
  let givensTyConSubst = mkTyConSubst givens
  unit_wanteds <- catMaybes <$> mapM (runMaybeT . toSolverConstraint defs givensTyConSubst) wanteds
  case unit_wanteds of
    [] -> return (TcPluginOk [] [])
    _  -> do
      unit_givens <- catMaybes <$> mapM (runMaybeT . toSolverConstraint defs givensTyConSubst) givens
      sr <- simplifyExtra defs (unit_givens ++ unit_wanteds)
      tcPluginTrace "ghc-typelits-extra Wanteds {" $
        vcat [ text "givens:" <+> ppr givens
             , text "unit_givens" <+> ppr unit_givens
             , text $ replicate 80 '-'
             , text "wanteds:" <+> ppr wanteds
             , text "unit_wanteds:" <+> ppr unit_wanteds
             ]
      tcPluginTrace "normalised" (ppr sr)
      case sr of
        Simplified evs new -> return (TcPluginOk (filter (isWantedCt . snd) evs) new)
        Impossible eq  -> return (TcPluginContradiction [fromSolverConstraint eq])

data SolverConstraint
   = NatEquality Ct ExtraOp ExtraOp Normalised
   | NatInequality Ct [Coercion] ExtraOp ExtraOp Bool Normalised

instance Outputable SolverConstraint where
  ppr (NatEquality ct op1 op2 norm) =
    text "NatEquality" $$ ppr ct $$ ppr op1 $$ ppr op2 $$ ppr norm
  ppr (NatInequality _ _ op1 op2 b norm) =
    text "NatInequality" $$ ppr op1 $$ ppr op2 $$ ppr b $$ ppr norm

data SimplifyResult
  = Simplified [(EvTerm,Ct)] [Ct]
  | Impossible SolverConstraint

instance Outputable SimplifyResult where
  ppr (Simplified evs new) =
    text "Simplified" $$ text "Solved:" $$ ppr evs $$ text "New:" $$ ppr new
  ppr (Impossible sct) =
    text "Impossible" <+> ppr sct

simplifyExtra :: ExtraDefs -> [SolverConstraint] -> TcPluginM 'Solve SimplifyResult
simplifyExtra defs eqs = tcPluginTrace "simplifyExtra" (ppr eqs) >> simples [] [] eqs
  where
    simples :: [Maybe (EvTerm, Ct)] -> [Ct] -> [SolverConstraint] -> TcPluginM 'Solve SimplifyResult
    simples evs news [] = return (Simplified (catMaybes evs) news)
    simples evs news (eq@(NatEquality ct u v norm):eqs') = do
      let evM = evMagic (ordTyCons defs) ct (depsFromNormalised norm)
          wz = case (u, v) of
                 (CLogWZ a b _, CLog a' b') | a == a' && b == b' -> Just b
                 (CLog a' b', CLogWZ a b _) | a == a' && b == b' -> Just b
                 _ -> Nothing
      case wz of
        Just x -> do
          let eq' = NatInequality ct [] (I 1) x True norm
          newCt <- createWantedFromNormalised defs eq'
          simples (fmap (,ct) evM:evs) (newCt:news) eqs'
        Nothing -> do
          ur <- unifyExtra ct u v
          tcPluginTrace "unifyExtra result" (ppr ur)
          case ur of
            Win -> simples (fmap (,ct) evM:evs) news eqs'
            Lose | null evs && null eqs' -> return (Impossible eq)
            _ | Normalised {} <- norm
              , isWantedCt ct -> do
              newCt <- createWantedFromNormalised defs eq
              simples (fmap (,ct) evM:evs) (newCt:news) eqs'
            Lose -> simples evs news eqs'
            Draw -> simples evs news eqs'
    simples evs news (eq@(NatInequality ct deps u v b norm):eqs') = do
      tcPluginTrace "unifyExtra leq result" (ppr (u,v,b))
      let evM = evMagic (ordTyCons defs) ct (deps <> depsFromNormalised norm)
      case (u,v) of
        (I i,I j)
          | (i <= j) == b
          -> simples (fmap (,ct) evM:evs) news eqs'
          | otherwise     -> return  (Impossible eq)
        (p, Max x y)
          | b && (p == x || p == y)
          -> simples (fmap (,ct) evM:evs) news eqs'

        -- transform:  q ~ Max x y => (p <=? q ~ True)
        -- to:         (p <=? Max x y) ~ True
        -- and try to solve that along with the rest of the eqs'
        (p, q@(V _))
          | b -> case findMax q eqs of
                   Just (i,m) ->
                      simples evs news
                        (NatInequality ct (i:deps) p m b norm:eqs')
                   Nothing -> simples evs news eqs'
        _ | Normalised {} <- norm
          , isWantedCt ct -> do
          newCt <- createWantedFromNormalised defs eq
          simples (fmap (,ct) evM:evs) (newCt:news) eqs'
        _ -> simples evs news eqs'

    -- look for given constraint with the form: c ~ Max x y
    findMax :: ExtraOp -> [SolverConstraint] -> Maybe (Coercion, ExtraOp)
    findMax c = go
      where
        go [] = Nothing
        go ((NatEquality ct a b@(Max _ _) _) :_)
          | c == a && not (isWantedCt ct)
            = Just (ctEvCoercion (ctEvidence ct), b)
        go ((NatEquality ct a@(Max _ _) b _) :_)
          | c == b && not (isWantedCt ct)
            = Just (ctEvCoercion (ctEvidence ct), a)
        go (_:rest) = go rest


-- Extract the Nat equality constraints
toSolverConstraint :: ExtraDefs -> TyConSubst -> Ct -> MaybeT (TcPluginM 'Solve) SolverConstraint
toSolverConstraint defs givensTyConSubst ct =
    case isNatRel (ordTyCons defs) givensTyConSubst ty0 of
      Nothing -> fail "Nothing"
      Just (((t1,t2),leqM),deps) -> do
        (t1', n1) <- normaliseNat defs t1
        (t2', n2) <- normaliseNat defs t2
        case leqM of
          Nothing ->
            pure (NatEquality ct t1' t2' (mergeNormalised n1 n2))
          Just b ->
            pure (NatInequality ct deps t1' t2' b (mergeNormalised n1 n2))
  where
   ty0 = ctEvPred (ctEvidence ct)

createWantedFromNormalised :: ExtraDefs -> SolverConstraint -> TcPluginM 'Solve Ct
createWantedFromNormalised defs sct = do
  let extractCtSides (NatEquality ct t1 t2 _)   = (ct, reifyEOP defs t1, reifyEOP defs t2)
      extractCtSides (NatInequality ct _ x y b _) =
        let t1 = mkLeqQNat (ordTyCons defs) (reifyEOP defs x) (reifyEOP defs y)
            tb = if b then promotedTrueDataCon else promotedFalseDataCon
            t2 = TyConApp tb []
          in (ct, t1, t2)
  let (ct, t1, t2) = extractCtSides sct
  newPredTy <- toLeqPredType (ordTyCons defs) ct t1 t2
  ev <- newWanted (ctLoc ct) newPredTy
  return (setCtEv ct ev)

fromSolverConstraint :: SolverConstraint -> Ct
fromSolverConstraint (NatEquality ct _ _ _)  = ct
fromSolverConstraint (NatInequality ct _ _ _ _ _) = ct

-- Utils
evMagic :: LookedUpTyCons -> Ct -> [Coercion] -> Maybe EvTerm
evMagic tcs ct deps = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2 ->
      let ctEv = mkPluginUnivCo "ghc-typelits-extra" Nominal deps t1 t2
       in Just (EvExpr (Coercion ctEv))
    IrredPred p ->
      let t1 = mkTyConApp (c0TyCon tcs) []
          co = mkPluginUnivCo "ghc-typelits-extra" Representational deps t1 p
          dcApp = evId (dataConWrapId (c0DataCon tcs))
       in Just (EvExpr (evCast dcApp co))
    _ -> Nothing
