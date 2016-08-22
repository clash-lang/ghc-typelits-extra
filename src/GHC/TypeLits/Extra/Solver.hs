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

{-# LANGUAGE TupleSections #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module GHC.TypeLits.Extra.Solver
  ( plugin )
where

-- external
import Control.Monad             ((<=<))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Maybe                (catMaybes)
import GHC.TcPluginM.Extra       (evByFiat, lookupModule, lookupName,
                                  tracePlugin)

-- GHC API
import FastString (fsLit)
import Module     (mkModuleName)
import OccName    (mkTcOcc)
import Outputable (Outputable (..), (<+>), ($$), text)
import Plugins    (Plugin (..), defaultPlugin)
import TcEvidence (EvTerm)
import TcPluginM  (TcPluginM, tcLookupTyCon, tcPluginTrace, zonkCt)
import TcRnTypes  (Ct, TcPlugin(..), TcPluginResult (..), ctEvidence, ctEvPred,
                   isWanted)
import TcType      (typeKind)
import Type       (EqRel (NomEq), Kind, PredTree (EqPred), classifyPredType,
                   eqType)
import TysWiredIn (typeNatKind)

-- internal
import GHC.TypeLits.Extra.Solver.Operations
import GHC.TypeLits.Extra.Solver.Unify

-- | A solver implement as a type-checker plugin for:
--
--     * 'Div': type-level 'div'
--
--     * 'Mod': type-level 'mod'
--
--     * 'FLog': type-level equivalent of <https://hackage.haskell.org/package/integer-gmp/docs/GHC-Integer-Logarithms.html#v:integerLogBase-35- integerLogBase#>
--       .i.e. the exact integer equivalent to "@'floor' ('logBase' x y)@"
--
--     * 'CLog': type-level equivalent of /the ceiling of/ <https://hackage.haskell.org/package/integer-gmp/docs/GHC-Integer-Logarithms.html#v:integerLogBase-35- integerLogBase#>
--       .i.e. the exact integer equivalent to "@'ceiling' ('logBase' x y)@"
--
--     * 'Log': type-level equivalent of <https://hackage.haskell.org/package/integer-gmp/docs/GHC-Integer-Logarithms.html#v:integerLogBase-35- integerLogBase#>
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
  unit_wanteds <- catMaybes <$> mapM (runMaybeT . toNatEquality defs) wanteds'
  case unit_wanteds of
    [] -> return (TcPluginOk [] [])
    _  -> do
      unit_givens <- catMaybes <$> mapM ((runMaybeT . toNatEquality defs) <=< zonkCt) givens
      sr <- simplifyExtra (unit_givens ++ unit_wanteds)
      tcPluginTrace "normalised" (ppr sr)
      case sr of
        Simplified evs -> return (TcPluginOk (filter (isWanted . ctEvidence . snd) evs) [])
        Impossible eq  -> return (TcPluginContradiction [fromNatEquality eq])

type NatEquality = (Ct,ExtraOp,ExtraOp)

data SimplifyResult
  = Simplified [(EvTerm,Ct)]
  | Impossible NatEquality

instance Outputable SimplifyResult where
  ppr (Simplified evs) = text "Simplified" $$ ppr evs
  ppr (Impossible eq)  = text "Impossible" <+> ppr eq

simplifyExtra :: [NatEquality] -> TcPluginM SimplifyResult
simplifyExtra eqs = tcPluginTrace "simplifyExtra" (ppr eqs) >> simples [] eqs
  where
    simples :: [Maybe (EvTerm, Ct)] -> [NatEquality] -> TcPluginM SimplifyResult
    simples evs [] = return (Simplified (catMaybes evs))
    simples evs (eq@((ct,u,v)):eqs') = do
      ur <- unifyExtra ct u v
      tcPluginTrace "unifyExtra result" (ppr ur)
      case ur of
        Win  -> simples (((,) <$> evMagic ct <*> pure ct):evs) eqs'
        Lose -> return  (Impossible eq)
        Draw -> simples evs eqs'

-- Extract the Nat equality constraints
toNatEquality :: ExtraDefs -> Ct -> MaybeT TcPluginM NatEquality
toNatEquality defs ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2
      | isNatKind (typeKind t1) || isNatKind (typeKind t2)
      -> (ct,,) <$> normaliseNat defs t1 <*> normaliseNat defs t2
    _ -> fail "Nothing"
  where
    isNatKind :: Kind -> Bool
    isNatKind = (`eqType` typeNatKind)

fromNatEquality :: NatEquality -> Ct
fromNatEquality (ct, _, _) = ct

lookupExtraDefs :: TcPluginM ExtraDefs
lookupExtraDefs = do
    md <- lookupModule myModule myPackage
    ExtraDefs <$> look md "Div"
              <*> look md "Mod"
              <*> look md "FLog"
              <*> look md "CLog"
              <*> look md "Log"
              <*> look md "GCD"
              <*> look md "LCM"
  where
    look md s = tcLookupTyCon =<< lookupName md (mkTcOcc s)
    myModule  = mkModuleName "GHC.TypeLits.Extra"
    myPackage = fsLit "ghc-typelits-extra"

-- Utils
evMagic :: Ct -> Maybe EvTerm
evMagic ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2 -> Just (evByFiat "ghc-typelits-extra" t1 t2)
    _                  -> Nothing
