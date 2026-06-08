module ZkFold.Cardano.Asterizm.Utils where

import           Control.Exception      (SomeException, catch, throwIO)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import           Data.Maybe             (fromJust)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V3     (fromBuiltin, toBuiltin)
import           PlutusTx               (CompiledCode, toBuiltinData)
import           Prelude
import           System.IO              (hPutStrLn, stderr)

import           ZkFold.Cardano.UPLC.Asterizm (AsterizmClientAction, AsterizmHashMode (..), buildCrosschainHash,
                                               buildHash)

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

-- | Hash an Asterizm message using the selected token-name hash mode.
hashMessage :: AsterizmHashMode -> BS.ByteString -> BS.ByteString
hashMessage RegularHash = fromBuiltin . buildHash . toBuiltin
hashMessage CrosschainHash = fromBuiltin . buildCrosschainHash . toBuiltin

-- | Minting redeemer selecting the same hash mode used off-chain.
hashModeRedeemer :: AsterizmHashMode -> GYRedeemer
hashModeRedeemer = redeemerFromPlutusData . toBuiltinData

-- | Client-policy redeemer selecting direction and hash mode.
clientActionRedeemer :: AsterizmClientAction -> GYRedeemer
clientActionRedeemer = redeemerFromPlutusData . toBuiltinData

-- | The minimal omni-chain token uses the empty Cardano token name.
omniTokenNameGY :: GYTokenName
omniTokenNameGY = fromJust $ tokenNameFromBS BS.empty

-- | Decode the amount from @abi.decode(payload, (uint, uint, uint))@.
tokenTransferAmount :: BS.ByteString -> Either String Integer
tokenTransferAmount msg
  | BS.length msg /= 208 = Left "Asterizm token-transfer message must be 208 bytes."
  | headerTxId /= payloadTxId = Left "Payload txId does not match message header txId."
  | amount <= 0 = Left "Token-transfer amount must be positive."
  | otherwise = Right amount
  where
    headerTxId = BS.take 32 $ BS.drop 80 msg
    payloadTxId = BS.take 32 $ BS.drop 176 msg
    amount = word256ToInteger . BS.take 32 $ BS.drop 144 msg

word256ToInteger :: BS.ByteString -> Integer
word256ToInteger = BS.foldl' (\acc w -> acc * 256 + fromIntegral w) 0

-- | Submit a signed transaction and print the CBOR hex when submission fails.
submitTxWithCborOnFailure :: GYNetworkId -> GYProviders -> User -> GYTx -> IO GYTxId
submitTxWithCborOnFailure nid providers user tx =
  runGYTxGameMonadIO nid providers (asUser user (submitTx tx)) `catch` printCborAndRethrow
  where
    printCborAndRethrow :: SomeException -> IO GYTxId
    printCborAndRethrow err = do
      hPutStrLn stderr "BEGIN_SIGNED_TX_CBOR"
      hPutStrLn stderr (txToHex tx)
      hPutStrLn stderr "END_SIGNED_TX_CBOR"
      throwIO err
