module ZkFold.Cardano.OnChain.Plonkup.Data where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import PlutusTx (makeLift)
import PlutusTx.Blueprint
import PlutusTx.Blueprint.TH qualified
import PlutusTx.Builtins (BuiltinByteString, Integer)
import Prelude (Show)

import ZkFold.Cardano.OnChain.BLS12_381.F (F)
import ZkFold.Cardano.OnChain.Orphans ()

data SetupBytes = SetupBytes
    { n :: Integer
    , nPrv :: Integer
    , pow :: Integer
    , omega :: F
    , omegaNPrv :: F
    , k1 :: F
    , k2 :: F
    , h1_bytes :: BuiltinByteString
    , cmQm_bytes :: BuiltinByteString
    , cmQl_bytes :: BuiltinByteString
    , cmQr_bytes :: BuiltinByteString
    , cmQo_bytes :: BuiltinByteString
    , cmQc_bytes :: BuiltinByteString
    , cmQk_bytes :: BuiltinByteString
    , cmS1_bytes :: BuiltinByteString
    , cmS2_bytes :: BuiltinByteString
    , cmS3_bytes :: BuiltinByteString
    , cmT1_bytes :: BuiltinByteString
    , cmT2_bytes :: BuiltinByteString
    , cmT3_bytes :: BuiltinByteString
    }
    deriving stock (Show, Generic)
    deriving anyclass (HasBlueprintDefinition, NFData)

makeLift ''SetupBytes
PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''SetupBytes [('SetupBytes, 0)]

type InputBytes = [F]

data ProofBytes = ProofBytes
    { cmA_bytes :: BuiltinByteString
    , cmB_bytes :: BuiltinByteString
    , cmC_bytes :: BuiltinByteString
    , cmF_bytes :: BuiltinByteString
    , cmH1_bytes :: BuiltinByteString
    , cmH2_bytes :: BuiltinByteString
    , cmZ1_bytes :: BuiltinByteString
    , cmZ2_bytes :: BuiltinByteString
    , cmQlow_bytes :: BuiltinByteString
    , cmQmid_bytes :: BuiltinByteString
    , cmQhigh_bytes :: BuiltinByteString
    , proof1_bytes :: BuiltinByteString
    , proof2_bytes :: BuiltinByteString
    , a_xi_int :: Integer
    , b_xi_int :: Integer
    , c_xi_int :: Integer
    , s1_xi_int :: Integer
    , s2_xi_int :: Integer
    , f_xi_int :: Integer
    , t_xi_int :: Integer
    , t_xi'_int :: Integer
    , z1_xi'_int :: Integer
    , z2_xi'_int :: Integer
    , h1_xi'_int :: Integer
    , h2_xi_int :: Integer
    , l1_xi :: F
    , l_xi :: [F]
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, HasBlueprintDefinition, NFData)

makeLift ''ProofBytes
PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''ProofBytes [('ProofBytes, 0)]
