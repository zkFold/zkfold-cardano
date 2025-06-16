module ZkFold.Cardano.Asterizm.Utils where

import           Data.Aeson.Types       (Parser)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as BS
import           GeniusYield.Types
import           PlutusTx               (CompiledCode)
import           Prelude


-- | Convert ByteString to hex-encoded String
bsToHex :: ByteString -> String
bsToHex = BS.unpack . B16.encode

-- | Decode hex-encoded String into ByteString
hexToBS :: String -> Parser ByteString
hexToBS s =
  case B16.decode $ BS.pack s of
    Left err -> fail $ "Invalid hex string: " ++ err
    Right bs -> pure bs

-- | Minting policy and policy ID from Plutus policy.
policyFromPlutus :: forall a. CompiledCode a -> (GYBuildScript PlutusV3, GYMintingPolicyId)
policyFromPlutus plutusPolicy = (policy, policyId)
  where
    mintScript = scriptFromPlutus @PlutusV3 plutusPolicy
    policy     = GYMintScript @PlutusV3 mintScript
    policyId   = mintingPolicyIdFromWitness policy
