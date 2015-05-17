{-# LANGUAGE CPP             #-}
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
import Data.Maybe (catMaybes, mapMaybe)

-- GHC API
import Coercion   (Role (Nominal), mkUnivCo)
import FastString (FastString, fsLit)
import Module     (Module, ModuleName, mkModuleName, moduleNameString)
import Name       (Name)
import OccName    (OccName, mkTcOcc)
import Outputable (Outputable (..), (<+>), ($$), empty, text)
import Plugins    (Plugin (..), defaultPlugin)
import TcEvidence (EvTerm (EvCoercion), TcCoercion (..))
import TcPluginM  (FindResult (Found), TcPluginM, findImportedModule,
                   lookupOrig, tcLookupTyCon, tcPluginTrace,
                   unsafeTcPluginTcM, zonkCt)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 711
import qualified  Inst
#else
import qualified  TcMType
#endif
import TcRnTypes  (Ct, CtLoc, CtOrigin, TcPlugin(..),
                   TcPluginResult(..), ctEvidence, ctEvPred,
                   ctLoc, ctLocOrigin, isGiven, isWanted, mkNonCanonical)
import TcSMonad   (runTcS,newGivenEvVar)
import TcType     (mkEqPred, typeKind)
import Type       (EqRel (NomEq), Kind, PredTree (EqPred), PredType, Type,
                   TyVar, classifyPredType, mkTyVarTy)
import TysWiredIn (typeNatKind)

-- internal
import GHC.TypeLits.Extra.Solver.Operations
import GHC.TypeLits.Extra.Solver.Unify

-- workaround for https://ghc.haskell.org/trac/ghc/ticket/10301
import Control.Monad (unless)
import Data.IORef    (readIORef)
import StaticFlags   (initStaticOpts, v_opt_C_ready)
import TcPluginM     (tcPluginIO)

-- | To use the plugin, add
--
-- @
-- {\-\# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise \#-\}
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
        Impossible eq  -> return (TcPluginContradiction [fromNatEquality eq])

substItemToCt :: ExtraDefs -> SubstItem -> TcPluginM Ct
substItemToCt defs si
  | isGiven (ctEvidence ct) = newSimpleGiven loc predicate (ty1,ty2)
  | otherwise               = newSimpleWanted (ctLocOrigin loc) predicate
  where
    predicate = mkEqPred ty1 ty2
    ty1  = mkTyVarTy (siVar si)
    ty2  = reifyExtraOp defs (siOP si)
    ct   = siNote si
    loc  = ctLoc ct

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
    g <- look md "GCD"
    return $ ExtraDefs g
  where
    look md s = tcLookupTyCon =<< lookupName md (mkTcOcc s)
    myModule  = mkModuleName "GHC.TypeLits.Extra"
    myPackage = fsLit "ghc-typelits-extra"

-- Utils
newSimpleWanted :: CtOrigin -> PredType -> TcPluginM Ct
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 711
newSimpleWanted orig = fmap mkNonCanonical . unsafeTcPluginTcM . Inst.newWanted orig
#else
newSimpleWanted orig = unsafeTcPluginTcM . TcMType.newSimpleWanted orig
#endif

newSimpleGiven :: CtLoc -> PredType -> (Type,Type) -> TcPluginM Ct
newSimpleGiven loc predicate (ty1,ty2)= do
  (ev,_) <- unsafeTcPluginTcM $ runTcS
                              $ newGivenEvVar loc
                                  (predicate, evByFiat "ghc-typelits-extra" (ty1, ty2))
  return (mkNonCanonical ev)

evMagic :: Ct -> Maybe EvTerm
evMagic ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2 -> Just (evByFiat "ghc-typelits-extra_magic" (t1, t2))
    _                  -> Nothing

evByFiat :: String -> (Type, Type) -> EvTerm
evByFiat name (t1,t2) = EvCoercion $ TcCoercion
                      $ mkUnivCo (fsLit name) Nominal t1 t2

tracePlugin :: String -> TcPlugin -> TcPlugin
tracePlugin s TcPlugin{..} = TcPlugin { tcPluginInit  = traceInit
                                      , tcPluginSolve = traceSolve
                                      , tcPluginStop  = traceStop
                                      }
  where
    traceInit    = do -- workaround for https://ghc.haskell.org/trac/ghc/ticket/10301
                      initializeStaticFlags
                      tcPluginTrace ("tcPluginInit " ++ s) empty >> tcPluginInit
    traceStop  z = do -- workaround for https://ghc.haskell.org/trac/ghc/ticket/10301
                      initializeStaticFlags
                      tcPluginTrace ("tcPluginStop " ++ s) empty >> tcPluginStop z

    traceSolve z given derived wanted = do
        -- workaround for https://ghc.haskell.org/trac/ghc/ticket/10301
        initializeStaticFlags
        tcPluginTrace ("tcPluginSolve start " ++ s)
                          (text "given   =" <+> ppr given
                        $$ text "derived =" <+> ppr derived
                        $$ text "wanted  =" <+> ppr wanted)
        r <- tcPluginSolve z given derived wanted
        case r of
          TcPluginOk solved new     -> tcPluginTrace ("tcPluginSolve ok " ++ s)
                                           (text "solved =" <+> ppr solved
                                         $$ text "new    =" <+> ppr new)
          TcPluginContradiction bad -> tcPluginTrace ("tcPluginSolve contradiction " ++ s)
                                           (text "bad =" <+> ppr bad)
        return r

lookupModule :: ModuleName -> FastString -> TcPluginM Module
lookupModule mod_nm pkg = do
    found_module <- findImportedModule mod_nm $ Just pkg
    case found_module of
      Found _ md -> return md
      _          -> do found_module' <- findImportedModule mod_nm $ Just $ fsLit "this"
                       case found_module' of
                         Found _ md -> return md
                         _          -> error $ "Unable to resolve module looked up by plugin: " ++ moduleNameString mod_nm

lookupName :: Module -> OccName -> TcPluginM Name
lookupName md occ = lookupOrig md occ

-- workaround for https://ghc.haskell.org/trac/ghc/ticket/10301
initializeStaticFlags :: TcPluginM ()
initializeStaticFlags = tcPluginIO $ do
  r <- readIORef v_opt_C_ready
  unless r initStaticOpts
