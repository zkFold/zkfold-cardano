module Main where

import           Cardano.Api         (SerialiseAsRawBytes (..), parsePolicyId)
import           Cardano.Api.Ledger  (toCBOR)
import           Codec.CBOR.Write    (toStrictByteString)
import           Data.ByteString     as BS (writeFile)
import           Prelude             (Either (..), IO, Show (..), head, print, ($), (++), (.), (<$>))
import           System.Environment  (getArgs)
import           Text.Parsec         (parse)

main :: IO ()
main = do
  policyidE <- parse parsePolicyId "" . head <$> getArgs

  case policyidE of
    Right policyid -> BS.writeFile "../assets/datumPlonk.cbor" $ toStrictByteString $ toCBOR $ serialiseToRawBytes policyid
    Left err       -> print $ "parse" ++ show err
