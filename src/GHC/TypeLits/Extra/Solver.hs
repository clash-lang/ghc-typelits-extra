{-# LANGUAGE CPP             #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Copyright  :  (C) 2015, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

-}
module GHC.TypeLits.Extra.Solver
  ( plugin )
where

-- external
import Data.Maybe          (catMaybes, mapMaybe)
import GHC.TcPluginM.Extra (evByFiat, failWithProvenace, lookupModule,
                            lookupName, newGiven, newWantedWithProvenance,
                            tracePlugin)

-- GHC API
import FastString (fsLit)
import Module     (mkModuleName)
import OccName    (mkTcOcc)
import Outputable (Outputable (..), (<+>), ($$), text)
import Plugins    (Plugin (..), defaultPlugin)
import TcEvidence (EvTerm)
import TcPluginM  (TcPluginM, tcLookupTyCon, tcPluginTrace, zonkCt)
import TcRnTypes  (Ct, TcPlugin(..), TcPluginResult (..), ctEvidence,
                   ctEvPred, ctLoc, isGiven, isWanted, mkNonCanonical)
import TcType     (mkEqPred, typeKind)
import Type       (EqRel (NomEq), Kind, PredTree (EqPred), classifyPredType,
                   mkTyVarTy)
import TysWiredIn (typeNatKind)

-- internal
import GHC.TypeLits.Extra.Solver.Operations
import GHC.TypeLits.Extra.Solver.Unify

-- | A solver implement as a type-checker plugin for:
--
--   * @GHC.TypeLits.Extra.GCD@: a type-level 'gcd'
--
--   * @GHC.TypeLits.Extra.CLog@: type-level equivalent of
--     \"@clog x y = 'ceiling' ('logBase' x y)@\"
--
-- To use the plugin, add
--
-- @
-- {\-\# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver \#-\}
-- @
--
-- To the header of your file.
plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const $ Just normalisePlugin }

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
  let wanteds' = filter (isWanted . ctEvidence) wanteds
  let unit_wanteds = mapMaybe (toNatEquality defs) wanteds'
  case unit_wanteds of
    [] -> return (TcPluginOk [] [])
    _  -> do
      unit_givens <- mapMaybe (toNatEquality defs) <$> mapM zonkCt givens
      sr <- simplifyExtra (unit_givens ++ unit_wanteds)
      tcPluginTrace "normalised" (ppr sr)
      case sr of
        Simplified subst evs ->
          TcPluginOk (filter (isWanted . ctEvidence . snd) evs) <$>
            mapM (substItemToCt defs) (filter (isWanted . ctEvidence . siNote) subst)
        Impossible eq -> failWithProvenace $ fromNatEquality eq

substItemToCt :: ExtraDefs -> SubstItem -> TcPluginM Ct
substItemToCt defs si
  | isGiven (ctEvidence ct) = mkNonCanonical <$> newGiven loc predicate evTm
  | otherwise               = mkNonCanonical <$> newWantedWithProvenance
                                                   (ctEvidence ct) predicate
  where
    predicate = mkEqPred ty1 ty2
    ty1  = mkTyVarTy (siVar si)
    ty2  = reifyExtraOp defs (siOP si)
    ct   = siNote si
    loc  = ctLoc ct
    evTm = evByFiat "ghc-typelits-extra" ty1 ty2

type NatEquality = (Ct,ExtraOp,ExtraOp)

data SimplifyResult
  = Simplified ExtraSubst [(EvTerm,Ct)]
  | Impossible NatEquality

instance Outputable SimplifyResult where
  ppr (Simplified subst evs) = text "Simplified" $$ ppr subst $$ ppr evs
  ppr (Impossible eq)  = text "Impossible" <+> ppr eq

simplifyExtra :: [NatEquality] -> TcPluginM SimplifyResult
simplifyExtra eqs = tcPluginTrace "simplifyExtra" (ppr eqs) >> simples [] [] [] eqs
  where
    simples :: ExtraSubst -> [Maybe (EvTerm, Ct)] -> [NatEquality]
            -> [NatEquality] -> TcPluginM SimplifyResult
    simples subst evs _xs [] = return (Simplified subst (catMaybes evs))
    simples subst evs xs (eq@(ct,u,v):eqs') = do
      ur <- unifyExtra ct (substsExtra subst u) (substsExtra subst v)
      tcPluginTrace "unifyExtra result" (ppr ur)
      case ur of
        Win         -> simples subst (((,) <$> evMagic ct <*> pure ct):evs) []
                               (xs ++ eqs')
        Lose        -> return  (Impossible eq)
        Draw []     -> simples subst evs (eq:xs) eqs'
        Draw subst' -> simples (substsSubst subst' subst ++ subst') evs [eq]
                               (xs ++ eqs')

-- Extract the Nat equality constraints
toNatEquality :: ExtraDefs -> Ct -> Maybe NatEquality
toNatEquality defs ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2
      | isNatKind (typeKind t1) || isNatKind (typeKind t1)
      -> (ct,,) <$> normaliseNat defs t1 <*> normaliseNat defs t2
    _ -> Nothing
  where
    isNatKind :: Kind -> Bool
    isNatKind = (== typeNatKind)

fromNatEquality :: NatEquality -> Ct
fromNatEquality (ct, _, _) = ct

lookupExtraDefs :: TcPluginM ExtraDefs
lookupExtraDefs = do
    md <- lookupModule myModule myPackage
    gcdTc <- look md "GCD"
    clogTc <- look md "CLog"
    return $ ExtraDefs gcdTc clogTc
  where
    look md s = tcLookupTyCon =<< lookupName md (mkTcOcc s)
    myModule  = mkModuleName "GHC.TypeLits.Extra"
    myPackage = fsLit "ghc-typelits-extra"

-- Utils
evMagic :: Ct -> Maybe EvTerm
evMagic ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2 -> Just (evByFiat "ghc-typelits-extra" t1 t2)
    _                  -> Nothing
