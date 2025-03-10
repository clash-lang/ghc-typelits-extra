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

{-# LANGUAGE CPP           #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module GHC.TypeLits.Extra.Solver
  ( plugin )
where

-- external
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Maybe                (catMaybes)
import GHC.TcPluginM.Extra       (evByFiat, lookupModule, lookupName
                                 ,tracePlugin, newWanted)
#if MIN_VERSION_ghc(8,4,0)
import GHC.TcPluginM.Extra (flattenGivens)
#else
import Control.Monad ((<=<))
#endif

-- GHC API
#if MIN_VERSION_ghc(9,0,0)
import GHC.Builtin.Names (eqPrimTyConKey, hasKey)
import GHC.Builtin.Types (promotedTrueDataCon, promotedFalseDataCon)
#if MIN_VERSION_ghc(9,2,0)
import GHC.Builtin.Types (boolTy, naturalTy)
#else
import GHC.Builtin.Types (typeNatKind)
#endif
import GHC.Builtin.Types.Literals (typeNatDivTyCon, typeNatModTyCon)
#if MIN_VERSION_ghc(9,2,0)
import GHC.Builtin.Types.Literals (typeNatCmpTyCon)
#else
import GHC.Builtin.Types.Literals (typeNatLeqTyCon)
#endif
import GHC.Core.Predicate (EqRel (NomEq), Pred (EqPred), classifyPredType)
import GHC.Core.TyCo.Rep (Type (..))
import GHC.Core.Type (Kind, eqType, mkTyConApp, splitTyConApp_maybe, typeKind)
import GHC.Data.FastString (fsLit)
import GHC.Driver.Plugins (Plugin (..), defaultPlugin, purePlugin)
import GHC.Tc.Plugin (TcPluginM, tcLookupTyCon, tcPluginTrace)
import GHC.Tc.Types (TcPlugin(..), TcPluginResult (..))
import GHC.Tc.Types.Constraint
  (Ct, ctEvidence, ctEvPred, ctLoc, isWantedCt, cc_ev)
#if MIN_VERSION_ghc(9,2,0)
import GHC.Tc.Types.Constraint (Ct (CQuantCan), qci_ev)
#endif
import GHC.Tc.Types.Evidence (EvTerm)
import GHC.Types.Name.Occurrence (mkTcOcc)
import GHC.Unit.Module (mkModuleName)
import GHC.Utils.Outputable (Outputable (..), (<+>), ($$), text)
#else
import FastString (fsLit)
import Module     (mkModuleName)
import OccName    (mkTcOcc)
import Outputable (Outputable (..), (<+>), ($$), text)
import Plugins    (Plugin (..), defaultPlugin)
#if MIN_VERSION_ghc(8,6,0)
import Plugins    (purePlugin)
#endif
import PrelNames  (eqPrimTyConKey, hasKey)
import TcEvidence (EvTerm)
import TcPluginM  (TcPluginM, tcLookupTyCon, tcPluginTrace)
import TcRnTypes  (TcPlugin(..), TcPluginResult (..))
import Type       (Kind, eqType, mkTyConApp, splitTyConApp_maybe)
import TyCoRep    (Type (..))
import TysWiredIn (typeNatKind, promotedTrueDataCon, promotedFalseDataCon)
import TcTypeNats (typeNatLeqTyCon)
#if MIN_VERSION_ghc(8,4,0)
import TcTypeNats (typeNatDivTyCon, typeNatModTyCon)
#else
import TcPluginM  (zonkCt)
#endif

#if MIN_VERSION_ghc(8,10,0)
import Constraint (Ct, ctEvidence, ctEvPred, ctLoc, isWantedCt, cc_ev)
import Predicate  (EqRel (NomEq), Pred (EqPred), classifyPredType)
import Type       (typeKind)
#else
import TcRnTypes  (Ct, ctEvidence, ctEvPred, ctLoc, isWantedCt, cc_ev)
import TcType     (typeKind)
import Type       (EqRel (NomEq), PredTree (EqPred), classifyPredType)
#endif
#endif

-- internal
import GHC.TypeLits.Extra.Solver.Operations
import GHC.TypeLits.Extra.Solver.Unify

#if MIN_VERSION_ghc(9,2,0)
typeNatKind :: Type
typeNatKind = naturalTy
#endif

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
#if MIN_VERSION_ghc(8,6,0)
  , pluginRecompile = purePlugin
#endif
  }

normalisePlugin :: TcPlugin
normalisePlugin = tracePlugin "ghc-typelits-extra"
  TcPlugin { tcPluginInit  = lookupExtraDefs
           , tcPluginSolve = decideEqualSOP
           , tcPluginStop  = const (return ())
           }

decideEqualSOP :: ExtraDefs -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
decideEqualSOP _    _givens _deriveds []      = return (TcPluginOk [] [])
decideEqualSOP defs givens  _deriveds wanteds = do
  -- GHC 7.10.1 puts deriveds with the wanteds, so filter them out
  let wanteds' = filter isWantedCt wanteds
  unit_wanteds <- catMaybes <$> mapM (runMaybeT . toSolverConstraint defs) wanteds'
  case unit_wanteds of
    [] -> return (TcPluginOk [] [])
    _  -> do
#if MIN_VERSION_ghc(8,4,0)
      unit_givens <- catMaybes <$> mapM (runMaybeT . toSolverConstraint defs) (givens ++ flattenGivens givens)
#else
      unit_givens <- catMaybes <$> mapM ((runMaybeT . toSolverConstraint defs) <=< zonkCt) givens
#endif
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
#if MIN_VERSION_ghc(9,2,0)
      | TyConApp tc [_,cmpNat,TyConApp tt1 [],TyConApp tt2 [],TyConApp ff1 []] <- t1
      , tc == ordTyCon defs
      , TyConApp cmpNatTc [x,y] <- cmpNat
      , cmpNatTc == typeNatCmpTyCon
      , tt1 == promotedTrueDataCon
      , tt2 == promotedTrueDataCon
      , ff1 == promotedFalseDataCon
#else
      | TyConApp tc [x,y] <- t1
      , tc == typeNatLeqTyCon
#endif
      , TyConApp tc' [] <- t2
      -> do
          (x', n1) <- normaliseNat defs x
          (y', n2) <- normaliseNat defs y
          let res | tc' == promotedTrueDataCon  = pure (NatInequality ct x' y' True  (mergeNormalised n1 n2))
                  | tc' == promotedFalseDataCon = pure (NatInequality ct x' y' False (mergeNormalised n1 n2))
                  | otherwise                   = fail "Nothing"
          res
    _ -> fail "Nothing"
  where
    isNatKind :: Kind -> Bool
    isNatKind = (`eqType` typeNatKind)

createWantedFromNormalised :: ExtraDefs -> SolverConstraint -> TcPluginM Ct
createWantedFromNormalised defs sct = do
  let extractCtSides (NatEquality ct t1 t2 _)   = (ct, reifyEOP defs t1, reifyEOP defs t2)
      extractCtSides (NatInequality ct x y b _) =
        let tc = if b then promotedTrueDataCon else promotedFalseDataCon
#if MIN_VERSION_ghc(9,2,0)
            t1 = TyConApp (ordTyCon defs)
                    [ boolTy
                    , TyConApp typeNatCmpTyCon [reifyEOP defs x, reifyEOP defs y]
                    , TyConApp promotedTrueDataCon []
                    , TyConApp promotedTrueDataCon []
                    , TyConApp promotedFalseDataCon []
                    ]
#else
            t1 = TyConApp typeNatLeqTyCon [reifyEOP defs x, reifyEOP defs y]
#endif
            t2 = TyConApp tc []
          in (ct, t1, t2)
  let (ct, t1, t2) = extractCtSides sct
  newPredTy <- case splitTyConApp_maybe $ ctEvPred $ ctEvidence ct of
    Just (tc, [a, b, _, _]) | tc `hasKey` eqPrimTyConKey -> pure (mkTyConApp tc [a, b, t1, t2])
    _ -> fail "Nothing"
  ev <- newWanted (ctLoc ct) newPredTy
  let ctN = case ct of
#if MIN_VERSION_ghc(9,2,0)
              CQuantCan qc -> CQuantCan (qc { qci_ev = ev})
#endif
              ctX -> ctX { cc_ev = ev }
  return ctN

fromSolverConstraint :: SolverConstraint -> Ct
fromSolverConstraint (NatEquality ct _ _ _)  = ct
fromSolverConstraint (NatInequality ct _ _ _ _) = ct

lookupExtraDefs :: TcPluginM ExtraDefs
lookupExtraDefs = do
    md <- lookupModule myModule myPackage
#if MIN_VERSION_ghc(9,2,0)
    md2 <- lookupModule ordModule basePackage
#endif
    ExtraDefs <$> look md "Max"
              <*> look md "Min"
#if MIN_VERSION_ghc(8,4,0)
              <*> pure typeNatDivTyCon
              <*> pure typeNatModTyCon
#else
              <*> look md "Div"
              <*> look md "Mod"
#endif
              <*> look md "FLog"
              <*> look md "CLog"
              <*> look md "Log"
              <*> look md "GCD"
              <*> look md "LCM"
#if MIN_VERSION_ghc(9,2,0)
              <*> look md2 "OrdCond"
              <*> look md2 "OrdCond"
#else
              <*> pure typeNatLeqTyCon
              <*> pure typeNatLeqTyCon
#endif
              <*> look md "CLogWZ"
  where
    look md s = tcLookupTyCon =<< lookupName md (mkTcOcc s)
    myModule  = mkModuleName "GHC.TypeLits.Extra"
    myPackage = fsLit "ghc-typelits-extra"
#if MIN_VERSION_ghc(9,2,0)
    ordModule   = mkModuleName "Data.Type.Ord"
    basePackage = fsLit "base"
#endif

-- Utils
evMagic :: Ct -> Maybe EvTerm
evMagic ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2 -> Just (evByFiat "ghc-typelits-extra" t1 t2)
    _                  -> Nothing
