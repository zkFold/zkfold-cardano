module ZkFold.Cardano.Asterizm.Utils where

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           GeniusYield.Types
import           PlutusTx               (CompiledCode)
import           Prelude

-- | Minting policy and policy ID from Plutus policy.
policyFromPlutus :: forall a. CompiledCode a -> (GYBuildScript PlutusV3, GYMintingPolicyId)
policyFromPlutus plutusPolicy = (policy, policyId)
  where
    mintScript = scriptFromPlutus @PlutusV3 plutusPolicy
    policy     = GYMintScript @PlutusV3 mintScript
    policyId   = mintingPolicyIdFromWitness policy

-- | Convert ByteString to hex Text.
bsToHex :: BS.ByteString -> T.Text
bsToHex = TE.decodeUtf8 . B16.encode

-- | Parse hex Text to ByteString.
hexToBS :: MonadFail m => T.Text -> m BS.ByteString
hexToBS t = case B16.decode (TE.encodeUtf8 t) of
  Left err -> fail $ "Invalid hex: " ++ err
  Right bs -> pure bs
