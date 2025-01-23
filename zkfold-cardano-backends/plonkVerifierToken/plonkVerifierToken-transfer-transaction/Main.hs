module Main where

import           Cardano.Api                   (SerialiseAsRawBytes (..), parsePolicyId)
import           Data.ByteString               as BS (writeFile)
import           Prelude                       (Either (..), IO, Show (..), head, print, ($), (++), (.), (<$>))
import           System.Environment            (getArgs)
import           Text.Parsec                   (parse)

import           ZkFold.Cardano.OffChain.Utils (toPolicyid)

main :: IO ()
main = do
  policyidE <- parse parsePolicyId "" . head <$> getArgs

  case policyidE of
    Right policyid -> BS.writeFile "../assets/datumPlonkVerifierToken.cbor" $ toPolicyid policyid
    Left err       -> print $ "parse" ++ show err
