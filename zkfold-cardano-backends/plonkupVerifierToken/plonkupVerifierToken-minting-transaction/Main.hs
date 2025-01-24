module Main where

import           Cardano.Api                           (AssetName (..), UsingRawBytesHex (..))
import           Data.Aeson                            (decode)
import           Data.ByteString                       as BS (writeFile)
import qualified Data.ByteString.Lazy                  as BL
import           Data.Maybe                            (fromJust)
import           Data.String                           (IsString (..))
import           PlutusLedgerApi.V3                    (fromBuiltin)
import           Prelude                               (IO, putStr, show, ($), (++), (.), (<$>))

import           ZkFold.Cardano.Examples.EqualityCheck (EqualityCheckContract (..), equalityCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils         (dataToCBOR)
import qualified ZkFold.Cardano.OnChain.BLS12_381.F    as F
import           ZkFold.Cardano.OnChain.Plonkup.Data   (ProofBytes (..))

dummyRedeemer :: ProofBytes
dummyRedeemer = ProofBytes e e e e e e e e e e e e e 0 0 0 0 0 0 0 0 0 0 0 0 (F.F 0)
  where e = ""

main :: IO ()
main = do
  EqualityCheckContract{..} <- fromJust . decode <$> BL.readFile "../test-data/plonkup-raw-contract-data.json"

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n" ++ "targetValue: " ++ show targetValue ++ "\n"

  let (_, input, proof) = equalityCheckVerificationBytes x ps targetValue

  BS.writeFile "../assets/tokenname" $ fromString $ show $ UsingRawBytesHex $ AssetName $ fromBuiltin $ F.fromInput input
  BS.writeFile "../assets/unit.cbor" $ dataToCBOR ()
  BS.writeFile "../assets/redeemerPlonkupVerifierToken.cbor" $ dataToCBOR proof
  BS.writeFile "../assets/dummy-redeemer.cbor" $ dataToCBOR dummyRedeemer
