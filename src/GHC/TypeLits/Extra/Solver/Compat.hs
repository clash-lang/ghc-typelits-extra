{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}

module GHC.TypeLits.Extra.Solver.Compat where

import qualified Language.Haskell.TH as TH

import GHC.TcPlugin.API

import GHC.TypeLits.Normalise.Compat

import GHC.Builtin.Names
  ( eqPrimTyConKey, hasKey
  )
#if MIN_VERSION_ghc(9,1,0)
import GHC.Builtin.Types
  ( boolTy, promotedFalseDataCon, promotedTrueDataCon
  )
#endif
import GHC.Builtin.Types.Literals
  ( typeNatDivTyCon, typeNatModTyCon
  )
#if MIN_VERSION_ghc(9,8,0)
import GHC.Tc.Types.Constraint
  ( DictCt(..), EqCt(..), IrredCt(..)
  )
#endif
import GHC.Tc.Types.Constraint
  ( Ct (..), qci_ev
  )

import qualified GHC.TypeLits.Extra

data ExtraDefs = ExtraDefs
  { maxTyCon    :: TyCon
  , minTyCon    :: TyCon
  , divTyCon    :: TyCon
  , modTyCon    :: TyCon
  , flogTyCon   :: TyCon
  , clogTyCon   :: TyCon
  , clogWZTyCon :: TyCon
  , logTyCon    :: TyCon
  , gcdTyCon    :: TyCon
  , lcmTyCon    :: TyCon
  , ordTyCons   :: LookedUpTyCons
  }

-- | Find the \"magic\" classes and instances in "GHC.TypeLits.KnownNat"
lookupExtraDefs :: TcPluginM 'Init ExtraDefs
lookupExtraDefs = do
    ExtraDefs <$> look ''GHC.TypeLits.Extra.Max
              <*> look ''GHC.TypeLits.Extra.Min
              <*> pure typeNatDivTyCon
              <*> pure typeNatModTyCon
              <*> look ''GHC.TypeLits.Extra.FLog
              <*> look ''GHC.TypeLits.Extra.CLog
              <*> look ''GHC.TypeLits.Extra.CLogWZ
              <*> look ''GHC.TypeLits.Extra.Log
              <*> look ''GHC.TypeLits.Extra.GCD
              <*> look ''GHC.TypeLits.Extra.LCM
              <*> lookupTyCons
  where
    look :: TH.Name -> TcPluginM 'Init TyCon
    look nm = tcLookupTyCon =<< lookupTHName nm

setCtEv :: Ct -> CtEvidence -> Ct
setCtEv ct ev = case ct of
  CQuantCan qc -> CQuantCan (qc { qci_ev = ev})
#if MIN_VERSION_ghc(9,8,0)
  CDictCan di     -> CDictCan (di { di_ev = ev})
  CIrredCan ir    -> CIrredCan (ir { ir_ev = ev})
  CEqCan eq       -> CEqCan (eq { eq_ev = ev})
  CNonCanonical _ -> CNonCanonical ev
#else
  ctX             -> ctX { cc_ev = ev }
#endif

mkLeqQNat :: LookedUpTyCons -> Type -> Type -> Type
mkLeqQNat tcs x y =
#if MIN_VERSION_ghc(9,1,0)
  mkTyConApp (ordCondTyCon tcs)
    [ boolTy
    , mkTyConApp (cmpNatTyCon tcs) [x,y]
    , mkTyConApp promotedTrueDataCon []
    , mkTyConApp promotedTrueDataCon []
    , mkTyConApp promotedFalseDataCon []
    ]
#else
  mkTyConApp (leqQNatTyCon tcs) [x, y]
#endif

toLeqPredType :: Monad m => LookedUpTyCons -> Ct -> Type -> Type -> m PredType
toLeqPredType defs ct t1 t2 = case splitTyConApp_maybe $ ctEvPred $ ctEvidence ct of
  Just (tc, [a, b, _, _]) | tc `hasKey` eqPrimTyConKey -> pure (mkTyConApp tc [a, b, t1, t2])
#if MIN_VERSION_ghc(9,3,0)
  Just (tc, [_, b]) | tc == assertTyCon defs -> pure (mkTyConApp tc [t1,b])
#endif
  _ -> error "Impossible: neither (<=?) nor Assert"
