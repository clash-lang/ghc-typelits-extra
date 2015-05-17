{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

import GHC.TypeLits.Extra
import Data.Proxy

test1 :: Proxy (GCD 6 8) -> Proxy 2
test1 = id
