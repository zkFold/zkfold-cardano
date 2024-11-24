{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cardano.Api                           (AssetName (..), UsingRawBytesHex (..), unsafeHashableScriptData)
import           Cardano.Api.Ledger                    (toCBOR)
import           Cardano.Api.Shelley                   (fromPlutusData, scriptDataToJsonDetailedSchema)
import           Codec.CBOR.Write                      (toStrictByteString)
import           Data.Aeson                            (decode)
import qualified Data.Aeson                            as Aeson
import           Data.ByteString                       as BS (ByteString, writeFile)
import qualified Data.ByteString.Lazy                  as BL
import           Data.Maybe                            (fromJust)
import           Data.String                           (IsString (..))
import           PlutusLedgerApi.V3                    (fromBuiltin)
import qualified PlutusLedgerApi.V3                    as V3
import           PlutusTx                              (ToData (..))
import           Prelude                               (IO, putStr, show, ($), (++), (.), (<$>))

import           ZkFold.Cardano.Examples.EqualityCheck (equalityCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.E2E           (EqualityCheckContract (..))
import qualified ZkFold.Cardano.OnChain.BLS12_381.F    as F
import           ZkFold.Cardano.OnChain.Plonkup.Data   (ProofBytes (..))

-- | Serialise data to CBOR and then wrap it in a JSON object.
dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . V3.toData

-- | Serialise data to CBOR.
dataToCBOR :: ToData a => a -> ByteString
dataToCBOR = toStrictByteString . toCBOR . fromPlutusData . V3.toData

dummyRedeemer :: ProofBytes
dummyRedeemer = ProofBytes e e e e e e e e e e e e e 0 0 0 0 0 0 0 0 0 0 0 0 (F.F 0)
  where e = ""

main :: IO ()
main = do
  EqualityCheckContract{..} <- fromJust . decode <$> BL.readFile "../test-data/plonk-raw-contract-data.json"

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n" ++ "targetValue: " ++ show targetValue ++ "\n"

  let (_, input, proof) = equalityCheckVerificationBytes x ps targetValue

  BS.writeFile "../assets/tokenname" $ fromString $ show $ UsingRawBytesHex $ AssetName $ fromBuiltin $ F.fromInput input
  BS.writeFile "../assets/unit.cbor" $ dataToCBOR ()
  BS.writeFile "../assets/redeemerPlonkVerifierToken.cbor" $ dataToCBOR proof
  BS.writeFile "../assets/dummy-redeemer.cbor" $ dataToCBOR dummyRedeemer
