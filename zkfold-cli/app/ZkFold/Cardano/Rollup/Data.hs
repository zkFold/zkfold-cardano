module ZkFold.Cardano.Rollup.Data where

import           Data.Coerce                  (coerce)
import           GeniusYield.Types
import           PlutusLedgerApi.V1.Value     (Lovelace (..))
import           PlutusLedgerApi.V3           (TxOut (..), DatumHash (..), OutputDatum (..))
import           PlutusTx.Builtins            (BuiltinByteString, ByteOrder (..), blake2b_256, integerToByteString)
import           Prelude                      (IO, Int, Integer, Maybe (..), Monad (..), ($), (.))
import           System.Random                (randomRIO)

import           ZkFold.Cardano.OnChain.Utils (dataToBlake)
import           ZkFold.Cardano.UPLC.Common   (parkingSpotCompiled)


rollupFee :: Lovelace
rollupFee = Lovelace 15000000

threadLovelace :: Lovelace
threadLovelace = Lovelace 3000000

updateLength :: Int
updateLength = 3

rmax :: Integer
rmax = 1000

minReq :: Lovelace
minReq = Lovelace 995610

evolve :: [BuiltinByteString] -> IO [BuiltinByteString]
evolve bs = do
  n <- randomRIO (1, rmax)
  return $ dataToBlake n : bs

-- | Bridge example
bridgeOut :: GYNetworkId -> Integer -> (GYTxOut PlutusV3, TxOut)
bridgeOut nid tag = (bridgeTxOut, bridgeTxOut')
  where
    bridgeTo    = scriptFromPlutus @PlutusV3 $ parkingSpotCompiled tag
    bridgeTxOut = GYTxOut { gyTxOutAddress = addressFromValidator nid bridgeTo
                          , gyTxOutValue   = valueFromLovelace $ coerce minReq
                          , gyTxOutDatum   = Just (datumFromPlutusData datumBSEx1, GYTxOutDontUseInlineDatum)
                          , gyTxOutRefS    = Nothing
                          }

    bridgeTxOut' = TxOut { txOutAddress         = addressToPlutus $ gyTxOutAddress bridgeTxOut
                         , txOutValue           = valueToPlutus $ gyTxOutValue bridgeTxOut
                         , txOutDatum           = OutputDatumHash . datumHashToPlutus . hashDatum $ datumFromPlutusData datumBSEx1
                         , txOutReferenceScript = Nothing
                         }

-- Datum hash example
datumBSEx1 :: BuiltinByteString
datumBSEx1 = integerToByteString BigEndian 0 43

datumHashBSEx1 :: BuiltinByteString
datumHashBSEx1 = blake2b_256 datumBSEx1

datumHashEx1 :: DatumHash
datumHashEx1 = DatumHash datumHashBSEx1
