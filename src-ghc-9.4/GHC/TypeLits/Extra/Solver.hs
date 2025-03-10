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

{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module GHC.TypeLits.Extra.Solver
  ( plugin )
where

-- external
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Maybe (catMaybes)
import GHC.TcPluginM.Extra (evByFiat, tracePlugin, newWanted)
import qualified Data.Type.Ord
import qualified GHC.TypeError

-- GHC API
import GHC.Builtin.Names (eqPrimTyConKey, hasKey, getUnique)
import GHC.Builtin.Types (promotedTrueDataCon, promotedFalseDataCon)
import GHC.Builtin.Types (boolTy, naturalTy, cTupleDataCon, cTupleTyCon)
import GHC.Builtin.Types.Literals (typeNatDivTyCon, typeNatModTyCon, typeNatCmpTyCon)
import GHC.Core.Coercion (mkUnivCo)
import GHC.Core.DataCon (dataConWrapId)
import GHC.Core.Predicate (EqRel (NomEq), Pred (EqPred, IrredPred), classifyPredType)
import GHC.Core.Reduction (Reduction(..))
import GHC.Core.TyCon (TyCon)
import GHC.Core.TyCo.Rep (Type (..), TyLit (..), UnivCoProvenance (PluginProv))
import GHC.Core.Type (Kind, mkTyConApp, splitTyConApp_maybe, typeKind)
#if MIN_VERSION_ghc(9,6,0)
import GHC.Core.TyCo.Compare (eqType)
#else
import GHC.Core.Type (eqType)
#endif
import GHC.Data.IOEnv (getEnv)
import GHC.Driver.Env (hsc_NC)
import GHC.Driver.Plugins (Plugin (..), defaultPlugin, purePlugin)
import GHC.Plugins (thNameToGhcNameIO)
import GHC.Tc.Plugin (TcPluginM, tcLookupTyCon, tcPluginTrace, tcPluginIO, unsafeTcPluginTcM)
import GHC.Tc.Types (TcPlugin(..), TcPluginSolveResult (..), TcPluginRewriter, TcPluginRewriteResult (..), Env (env_top))
import GHC.Tc.Types.Constraint
  (Ct, ctEvidence, ctEvPred, ctLoc, isWantedCt)
#if MIN_VERSION_ghc(9,8,0)
import GHC.Tc.Types.Constraint (Ct (..), DictCt(..), EqCt(..), IrredCt(..), qci_ev)
#else
import GHC.Tc.Types.Constraint (Ct (CQuantCan), qci_ev, cc_ev)
#endif
import GHC.Tc.Types.Evidence (EvTerm, EvBindsVar, Role(..), evCast, evId)
import GHC.Types.Unique.FM (UniqFM, listToUFM)
import GHC.Utils.Outputable (Outputable (..), (<+>), ($$), text)
import GHC (Name)

-- template-haskell
import qualified Language.Haskell.TH as TH

-- internal
import GHC.TypeLits.Extra.Solver.Operations
import GHC.TypeLits.Extra.Solver.Unify
import GHC.TypeLits.Extra

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
  { tcPlugin = const $ Just normalisePlugin
  , pluginRecompile = purePlugin
  }

normalisePlugin :: TcPlugin
normalisePlugin = tracePlugin "ghc-typelits-extra"
  TcPlugin { tcPluginInit    = lookupExtraDefs
           , tcPluginSolve   = decideEqualSOP
           , tcPluginRewrite = extraRewrite
           , tcPluginStop    = const (return ())
           }

extraRewrite :: ExtraDefs -> UniqFM TyCon TcPluginRewriter
extraRewrite defs = listToUFM
  [ (gcdTyCon defs, gcdRewrite)
  , (lcmTyCon defs, lcmRewrite)
  ]
  where
    gcdRewrite _ _ args@[LitTy (NumTyLit i), LitTy (NumTyLit j)] = pure $
      TcPluginRewriteTo (reduce (gcdTyCon defs) args (LitTy (NumTyLit (i `gcd` j)))) []
    gcdRewrite _ _ _ = pure TcPluginNoRewrite

    lcmRewrite _ _ args@[LitTy (NumTyLit i), LitTy (NumTyLit j)] = pure $
      TcPluginRewriteTo (reduce (lcmTyCon defs) args (LitTy (NumTyLit (i `lcm` j)))) []
    lcmRewrite _ _ _ = pure TcPluginNoRewrite

    reduce tc args res = Reduction co res
     where
      co = mkUnivCo (PluginProv "ghc-typelits-extra") Nominal
             (mkTyConApp tc args) res


decideEqualSOP :: ExtraDefs -> EvBindsVar -> [Ct] -> [Ct] -> TcPluginM TcPluginSolveResult
decideEqualSOP _    _ _givens []      = return (TcPluginOk [] [])
decideEqualSOP defs _ givens  wanteds = do
  unit_wanteds <- catMaybes <$> mapM (runMaybeT . toSolverConstraint defs) wanteds
  case unit_wanteds of
    [] -> return (TcPluginOk [] [])
    _  -> do
      unit_givens <- catMaybes <$> mapM (runMaybeT . toSolverConstraint defs) givens
      sr <- simplifyExtra defs (unit_givens ++ unit_wanteds)
      tcPluginTrace "normalised" (ppr sr)
      case sr of
        Simplified evs new -> return (TcPluginOk (filter (isWantedCt . snd) evs) new)
        Impossible eq  -> return (TcPluginContradiction [fromSolverConstraint eq])

data SolverConstraint
   = NatEquality Ct ExtraOp ExtraOp Normalised
   | NatInequality Ct ExtraOp ExtraOp Bool Normalised

instance Outputable SolverConstraint where
  ppr (NatEquality ct op1 op2 norm) = text "NatEquality" $$ ppr ct $$ ppr op1 $$ ppr op2 $$ ppr norm
  ppr (NatInequality _ op1 op2 b norm) = text "NatInequality" $$ ppr op1 $$ ppr op2 $$ ppr b $$ ppr norm

data SimplifyResult
  = Simplified [(EvTerm,Ct)] [Ct]
  | Impossible SolverConstraint

instance Outputable SimplifyResult where
  ppr (Simplified evs new) = text "Simplified" $$ text "Solved:" $$ ppr evs $$ text "New:" $$ ppr new
  ppr (Impossible sct)  = text "Impossible" <+> ppr sct

simplifyExtra :: ExtraDefs -> [SolverConstraint] -> TcPluginM SimplifyResult
simplifyExtra defs eqs = tcPluginTrace "simplifyExtra" (ppr eqs) >> simples [] [] eqs
  where
    simples :: [Maybe (EvTerm, Ct)] -> [Ct] -> [SolverConstraint] -> TcPluginM SimplifyResult
    simples evs news [] = return (Simplified (catMaybes evs) news)
    simples evs news (eq@(NatEquality ct u v norm):eqs') = do
      ur <- unifyExtra ct u v
      tcPluginTrace "unifyExtra result" (ppr ur)
      case ur of
        Win                          -> simples (((,) <$> evMagic ct <*> pure ct):evs) news eqs'
        Lose | null evs && null eqs' -> return (Impossible eq)
        _ | norm == Normalised && isWantedCt ct -> do
          newCt <- createWantedFromNormalised defs eq
          simples (((,) <$> evMagic ct <*> pure ct):evs) (newCt:news) eqs'
        Lose -> simples evs news eqs'
        Draw -> simples evs news eqs'
    simples evs news (eq@(NatInequality ct u v b norm):eqs') = do
      tcPluginTrace "unifyExtra leq result" (ppr (u,v,b))
      case (u,v) of
        (I i,I j)
          | (i <= j) == b -> simples (((,) <$> evMagic ct <*> pure ct):evs) news eqs'
          | otherwise     -> return  (Impossible eq)
        (p, Max x y)
          | b && (p == x || p == y) -> simples (((,) <$> evMagic ct <*> pure ct):evs) news eqs'

        -- transform:  q ~ Max x y => (p <=? q ~ True)
        -- to:         (p <=? Max x y) ~ True
        -- and try to solve that along with the rest of the eqs'
        (p, q@(V _))
          | b -> case findMax q eqs of
                   Just m  -> simples evs news (NatInequality ct p m b norm:eqs')
                   Nothing -> simples evs news eqs'
        _ | norm == Normalised && isWantedCt ct -> do
          newCt <- createWantedFromNormalised defs eq
          simples (((,) <$> evMagic ct <*> pure ct):evs) (newCt:news) eqs'
        _ -> simples evs news eqs'

    -- look for given constraint with the form: c ~ Max x y
    findMax :: ExtraOp -> [SolverConstraint] -> Maybe ExtraOp
    findMax c = go
      where
        go [] = Nothing
        go ((NatEquality ct a b@(Max _ _) _) :_)
          | c == a && not (isWantedCt ct)
            = Just b
        go ((NatEquality ct a@(Max _ _) b _) :_)
          | c == b && not (isWantedCt ct)
            = Just a
        go (_:rest) = go rest


-- Extract the Nat equality constraints
toSolverConstraint :: ExtraDefs -> Ct -> MaybeT TcPluginM SolverConstraint
toSolverConstraint defs ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2
      | isNatKind (typeKind t1) || isNatKind (typeKind t2)
      -> do
         (t1', n1) <- normaliseNat defs t1
         (t2', n2) <- normaliseNat defs t2
         pure (NatEquality ct t1' t2' (mergeNormalised n1 n2))
      | TyConApp tc [_,cmpNat,TyConApp tt1 [],TyConApp tt2 [],TyConApp ff1 []] <- t1
      , tc == ordTyCon defs
      , TyConApp cmpNatTc [x,y] <- cmpNat
      , cmpNatTc == typeNatCmpTyCon
      , tt1 == promotedTrueDataCon
      , tt2 == promotedTrueDataCon
      , ff1 == promotedFalseDataCon
      , TyConApp tc' [] <- t2
      -> do
          (x', n1) <- normaliseNat defs x
          (y', n2) <- normaliseNat defs y
          let res | tc' == promotedTrueDataCon  = pure (NatInequality ct x' y' True  (mergeNormalised n1 n2))
                  | tc' == promotedFalseDataCon = pure (NatInequality ct x' y' False (mergeNormalised n1 n2))
                  | otherwise                   = fail "Nothing"
          res
      | TyConApp tc [TyConApp ordCondTc zs, _] <- t1
      , tc == assertTC defs
      , TyConApp tc' [] <- t2
      , tc' == cTupleTyCon 0
      , ordCondTc == ordTyCon defs
      , [_,cmp,lt,eq,gt] <- zs
      , TyConApp tcCmpNat [x,y] <- cmp
      , tcCmpNat == typeNatCmpTyCon
      , TyConApp ltTc [] <- lt
      , ltTc == promotedTrueDataCon
      , TyConApp eqTc [] <- eq
      , eqTc == promotedTrueDataCon
      , TyConApp gtTc [] <- gt
      , gtTc == promotedFalseDataCon
      -> do
          (x', n1) <- normaliseNat defs x
          (y', n2) <- normaliseNat defs y
          pure (NatInequality ct x' y' True  (mergeNormalised n1 n2))
    IrredPred (TyConApp tc [TyConApp ordCondTc zs, _])
      | tc == assertTC defs
      , ordCondTc == ordTyCon defs
      , [_,cmp,lt,eq,gt] <- zs
      , TyConApp tcCmpNat [x,y] <- cmp
      , tcCmpNat == typeNatCmpTyCon
      , TyConApp ltTc [] <- lt
      , ltTc == promotedTrueDataCon
      , TyConApp eqTc [] <- eq
      , eqTc == promotedTrueDataCon
      , TyConApp gtTc [] <- gt
      , gtTc == promotedFalseDataCon
      -> do
          (x', n1) <- normaliseNat defs x
          (y', n2) <- normaliseNat defs y
          pure (NatInequality ct x' y' True  (mergeNormalised n1 n2))
    _ -> fail "Nothing"
  where
    isNatKind :: Kind -> Bool
    isNatKind = (`eqType` naturalTy)

createWantedFromNormalised :: ExtraDefs -> SolverConstraint -> TcPluginM Ct
createWantedFromNormalised defs sct = do
  let extractCtSides (NatEquality ct t1 t2 _)   = (ct, reifyEOP defs t1, reifyEOP defs t2)
      extractCtSides (NatInequality ct x y b _) =
        let tc = if b then promotedTrueDataCon else promotedFalseDataCon
            t1 = TyConApp (ordTyCon defs)
                    [ boolTy
                    , TyConApp typeNatCmpTyCon [reifyEOP defs x, reifyEOP defs y]
                    , TyConApp promotedTrueDataCon []
                    , TyConApp promotedTrueDataCon []
                    , TyConApp promotedFalseDataCon []
                    ]
            t2 = TyConApp tc []
          in (ct, t1, t2)
  let (ct, t1, t2) = extractCtSides sct
  newPredTy <- case splitTyConApp_maybe $ ctEvPred $ ctEvidence ct of
    Just (tc, [a, b, _, _]) | tc `hasKey` eqPrimTyConKey -> pure (mkTyConApp tc [a, b, t1, t2])
    Just (tc, [_, b]) | tc `hasKey` getUnique (assertTC defs) -> pure (mkTyConApp tc [t1,b])
    _ -> error "Impossible: neither (<=?) nor Assert"
  ev <- newWanted (ctLoc ct) newPredTy
  let ctN = case ct of
              CQuantCan qc -> CQuantCan (qc { qci_ev = ev})
#if MIN_VERSION_ghc(9,8,0)
              CDictCan di     -> CDictCan (di { di_ev = ev})
              CIrredCan ir    -> CIrredCan (ir { ir_ev = ev})
              CEqCan eq       -> CEqCan (eq { eq_ev = ev})
              CNonCanonical _ -> CNonCanonical ev
#else
              ctX -> ctX { cc_ev = ev }
#endif
  return ctN

fromSolverConstraint :: SolverConstraint -> Ct
fromSolverConstraint (NatEquality ct _ _ _)  = ct
fromSolverConstraint (NatInequality ct _ _ _ _) = ct

lookupExtraDefs :: TcPluginM ExtraDefs
lookupExtraDefs = do
    ExtraDefs <$> look ''GHC.TypeLits.Extra.Max
              <*> look ''GHC.TypeLits.Extra.Min
              <*> pure typeNatDivTyCon
              <*> pure typeNatModTyCon
              <*> look ''GHC.TypeLits.Extra.FLog
              <*> look ''GHC.TypeLits.Extra.CLog
              <*> look ''GHC.TypeLits.Extra.Log
              <*> look ''GHC.TypeLits.Extra.GCD
              <*> look ''GHC.TypeLits.Extra.LCM
              <*> look ''Data.Type.Ord.OrdCond
              <*> look ''GHC.TypeError.Assert
              <*> look ''GHC.TypeLits.Extra.CLogWZ
  where
    look nm = tcLookupTyCon =<< lookupTHName nm

lookupTHName :: TH.Name -> TcPluginM Name
lookupTHName th = do
    nc <- unsafeTcPluginTcM (hsc_NC . env_top <$> getEnv)
    res <- tcPluginIO $ thNameToGhcNameIO nc th
    maybe (fail $ "Failed to lookup " ++ show th) return res

-- Utils
evMagic :: Ct -> Maybe EvTerm
evMagic ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2 -> Just (evByFiat "ghc-typelits-extra" t1 t2)
    IrredPred p ->
      let t1 = mkTyConApp (cTupleTyCon 0) []
          co = mkUnivCo (PluginProv "ghc-typelits-extra") Representational t1 p
          dcApp = evId (dataConWrapId (cTupleDataCon 0))
       in Just (evCast dcApp co)
    _ -> Nothing
