module Main where

import           Cardano.Api         (SerialiseAsRawBytes (..), policyId)
import           Cardano.Api.Ledger  (toCBOR)
import           Codec.CBOR.Write    (toStrictByteString)
import           Data.ByteString     as BS (writeFile)
import           Prelude             (Either (..), IO, Show (..), head, print, ($), (++), (.), (<$>))
import           System.Environment  (getArgs)
import           Text.Parsec         (parse)

main :: IO ()
main = do
  policyidE <- parse policyId "" . head <$> getArgs

  case policyidE of
    Right policyid -> BS.writeFile "../../assets/datumPlonkupVerifierTx.cbor" $ toStrictByteString $ toCBOR $ serialiseToRawBytes policyid
    Left err       -> print $ "parse" ++ show err
