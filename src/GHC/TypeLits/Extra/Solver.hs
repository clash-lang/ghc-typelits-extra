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

#if __GLASGOW_HASKELL__ < 711
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

module GHC.TypeLits.Extra.Solver
  ( plugin )
where

-- external
import Control.Monad             ((<=<))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Maybe                (catMaybes)
import GHC.TcPluginM.Extra       (evByFiat, lookupModule, lookupName,
                                  tracePlugin)
#if __GLASGOW_HASKELL__ < 711
import GHC.TcPluginM.Extra       (failWithProvenace)
#endif

-- GHC API
import Class      (Class, classMethods, className, classTyCon)
import FamInst    (tcInstNewTyCon_maybe)
import FastString (fsLit)
import Id         (idType)
import Module     (mkModuleName)
import OccName    (mkTcOcc)
import Outputable (Outputable (..), (<+>), ($$), text)
import Plugins    (Plugin (..), defaultPlugin)
import PrelNames  (knownNatClassName)
import TcEvidence (EvTerm (EvLit), EvLit (EvNum), mkEvCast, mkTcSymCo, mkTcTransCo)
import TcPluginM  (TcPluginM, tcLookupTyCon, tcPluginTrace, zonkCt)
import TcRnTypes  (Ct, TcPlugin(..), TcPluginResult (..), ctEvidence, ctEvPred,
                   isWanted)
import TcType      (typeKind)
import Type       (EqRel (NomEq), Kind, PredTree (EqPred, ClassPred), Type, classifyPredType,
                   dropForAlls, eqType, funResultTy, tyConAppTyCon_maybe)
import TysWiredIn (typeNatKind)

-- internal
import GHC.TypeLits.Extra.Solver.Operations
import GHC.TypeLits.Extra.Solver.Unify

-- | A solver implement as a type-checker plugin for:
--
--   * @GHC.TypeLits.Extra.GCD@: a type-level 'gcd'
--
--   * @GHC.TypeLits.Extra.CLog@: type-level equivalent of /the ceiling of/ <https://hackage.haskell.org/package/integer-gmp/docs/GHC-Integer-Logarithms.html#v:integerLogBase-35- integerLogBase#>
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
#if __GLASGOW_HASKELL__ >= 711
        Impossible eq -> return (TcPluginContradiction [fromNatEquality eq])
#else
        Impossible eq -> failWithProvenace (fromNatEquality eq)
#endif

type NatEquality  = (Ct,ExtraOp,ExtraOp)
type KnConstraint = (Ct,Class,Type,ExtraOp)

data SimplifyResult
  = Simplified [(EvTerm,Ct)]
  | Impossible NatEquality

instance Outputable SimplifyResult where
  ppr (Simplified evs) = text "Simplified" $$ ppr evs
  ppr (Impossible eq)  = text "Impossible" <+> ppr eq

simplifyExtra :: [Either NatEquality KnConstraint] -> TcPluginM SimplifyResult
simplifyExtra eqs = tcPluginTrace "simplifyExtra" (ppr eqs) >> simples [] eqs
  where
    simples :: [Maybe (EvTerm, Ct)] -> [Either NatEquality KnConstraint] -> TcPluginM SimplifyResult
    simples evs [] = return (Simplified (catMaybes evs))
    simples evs (Left eq@((ct,u,v)):eqs') = do
      ur <- unifyExtra ct u v
      tcPluginTrace "unifyExtra result" (ppr ur)
      case ur of
        Win  -> simples (((,) <$> evMagic ct <*> pure ct):evs) eqs'
        Lose -> return  (Impossible eq)
        Draw -> simples evs eqs'
    simples evs (Right (ct,cls,ty,u):eqs') = do
      tcPluginTrace "unifyExtra KnownNat result" (ppr u)
      case u of
        (I i) -> simples (((,) <$> makeLitDict cls ty (EvNum i) <*> pure ct):evs) eqs'
        _ -> simples evs eqs'

-- Extract the Nat equality constraints
toNatEquality :: ExtraDefs -> Ct -> MaybeT TcPluginM (Either NatEquality KnConstraint)
toNatEquality defs ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2
      | isNatKind (typeKind t1) || isNatKind (typeKind t1)
      -> Left <$> ((ct,,) <$> normaliseNat defs t1 <*> normaliseNat defs t2)
    ClassPred cls [ty]
      | className cls == knownNatClassName
      -> Right <$> ((ct,cls,ty,) <$> normaliseNat defs ty)
    _ -> fail "Nothing"
  where
    isNatKind :: Kind -> Bool
    isNatKind = (`eqType` typeNatKind)

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

makeLitDict :: Class -> Type -> EvLit -> Maybe EvTerm
-- THIS CODE IS COPIED FROM:
-- https://github.com/ghc/ghc/blob/8035d1a5dc7290e8d3d61446ee4861e0b460214e/compiler/typecheck/TcInteract.hs#L1973
--
-- makeLitDict adds a coercion that will convert the literal into a dictionary
-- of the appropriate type.  See Note [KnownNat & KnownSymbol and EvLit]
-- in TcEvidence.  The coercion happens in 2 steps:
--
--     Integer -> SNat n     -- representation of literal to singleton
--     SNat n  -> KnownNat n -- singleton to dictionary
--
--     The process is mirrored for Symbols:
--     String    -> SSymbol n
--     SSymbol n -> KnownSymbol n -}
makeLitDict clas ty evLit
  | Just (_, co_dict) <- tcInstNewTyCon_maybe (classTyCon clas) [ty]
    -- co_dict :: KnownNat n ~ SNat n
  , [ meth ]   <- classMethods clas
  , Just tcRep <- tyConAppTyCon_maybe -- SNat
                    $ funResultTy     -- SNat n
                    $ dropForAlls     -- KnownNat n => SNat n
                    $ idType meth     -- forall n. KnownNat n => SNat n
  , Just (_, co_rep) <- tcInstNewTyCon_maybe tcRep [ty]
        -- SNat n ~ Integer
  , let ev_tm = mkEvCast (EvLit evLit) (mkTcSymCo (mkTcTransCo co_dict co_rep))
  = Just ev_tm
  | otherwise
  = Nothing
