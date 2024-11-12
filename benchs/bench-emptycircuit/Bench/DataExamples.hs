{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Bench.DataExamples where

import           Data.String        (IsString (fromString))
import qualified PlutusLedgerApi.V2 as V2
import           PlutusLedgerApi.V3
import           PlutusTx.Prelude   (Maybe (..), Integer, ($))
import           PlutusTx           (makeIsDataIndexed)

------- DATUM DATATYPES  ------

data Datum1 = Datum1
  { d1I :: Integer
  , d1B :: BuiltinByteString
  }

makeIsDataIndexed ''Datum1 [('Datum1,0)]

------- INPUTS  ------

input1 :: TxInInfo
input1 = TxInInfo
  { txInInfoOutRef = TxOutRef
    { txOutRefId  = TxId { getTxId = fromString "3dfe026b6e426d0678725f48e1d2138c8e159d7d2aaa6fc5afb27417ef330ba0" :: BuiltinByteString }
    , txOutRefIdx = 0
    }
  , txInInfoResolved = TxOut
    { txOutAddress = Address
      { addressCredential        = PubKeyCredential $
                                   PubKeyHash { getPubKeyHash = fromString "aabbccddeeff00112233445566778899aabbccddeeff001122334455" :: BuiltinByteString }
      , addressStakingCredential = Nothing
      }
    , txOutValue = lovelace 15000000
    , txOutDatum = OutputDatum (Datum { getDatum = toBuiltinData datum1Example })
    , txOutReferenceScript = Nothing
    }
  }
  where
    lovelace      = V2.singleton V2.adaSymbol V2.adaToken
    datum1Example = Datum1
      { d1I = 43
      , d1B = fromString "deadbeef" :: BuiltinByteString
      }
