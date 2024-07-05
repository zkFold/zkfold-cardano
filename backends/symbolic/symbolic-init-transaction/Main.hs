module Main where

import Prelude

{-
import           Cardano.Api                                 hiding (TxId)
import           Cardano.Api.Shelley                         (fromPlutusData, scriptDataToJsonDetailedSchema)
import           Cardano.Binary                              (serialize')
import           Codec.Serialise                             (Serialise (encode))
import           Data.Aeson                                  (decode)
import qualified Data.Aeson                                  as Aeson
import           Data.ByteString                             as BS (writeFile)
import qualified Data.ByteString.Base16                      as B16
import qualified Data.ByteString.Lazy                        as BL
import qualified PlutusLedgerApi.V3                          as V3
import           PlutusTx                                    (ToData (..))
import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)


import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Base.Protocol.ARK.Plonk              (Plonk (..))
import           ZkFold.Base.Protocol.ARK.Plonk.Internal     (getParams)
import           ZkFold.Base.Protocol.NonInteractiveProof    (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk.OffChain               (Contract (..), PlonkN, RowContractJSON, mkSetup, toContract)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit  (applyArgs)
import           ZkFold.Symbolic.Data.Bool                   (Bool (..))
import           ZkFold.Symbolic.Data.Eq                     (Eq (..))
import           Data.Map                                    (fromList, keys)
import           ZkFold.Base.Data.Vector                     (Vector (..))


dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . V3.toData

-}

main :: IO ()
main = do
  pure ()
  {-
  jsonRowContract <- BL.readFile "../test-data/raw-contract-data.json"
  let maybeRowContract = decode jsonRowContract :: Maybe RowContractJSON
  case maybeRowContract of
    Just rowContract ->
      let Contract{..} = toContract rowContract

          Bool ac = compile @Fr (lockedByTxId @(ArithmeticCircuit Fr) @Fr (TxId targetId))
          acc = applyArgs ac [targetId]

          (omega, k1, k2) = getParams 5

          inputs = fromList [(acOutput acc, 1 :: Integer)]

          setupV  = setupVerify @(PlonkN 32) $ Plonk omega k1 k2 (Vector @1 $ keys inputs) acc x
      in do
        let datum = DatumSetup $ mkSetup setupV
        BS.writeFile ".././assets/setup.json" $ prettyPrintJSON . dataToJSON $ datum
        BS.writeFile ".././assets/setup.cbor" $ B16.encode . serialize' . encode . V3.Datum . toBuiltinData $ datum
    _ -> print ("Could not deserialize" :: String)
  -}