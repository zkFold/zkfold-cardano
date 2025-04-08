module ZkFold.Cardano.UPLC.Wallet.Compile (
  writeSmartWalletBP,
  web2AuthSerialisedScript,
  web2AuthCompiledCode,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)
import Data.Function ((&))
import Data.Maybe (Maybe (..))
import Data.Set qualified as Set
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx qualified
import PlutusTx.Blueprint
import PlutusTx.Prelude qualified as PlutusTx (BuiltinUnit)
import ZkFold.Cardano.OnChain.Plonkup.Data (SetupBytes)
import ZkFold.Cardano.UPLC.Wallet
import Prelude (FilePath, IO, ($))

smartWalletBP :: ContractBlueprint
smartWalletBP =
  MkContractBlueprint
    { contractId = Just "smart-wallet"
    , contractPreamble =
        MkPreamble
          { preambleTitle = "Smart Wallet"
          , preambleDescription = Just "Validators related to zkFold's Smart Wallet. Visit https://zkfold.io/ for more details"
          , preambleVersion = "0.1.0"
          , preamblePlutusVersion = PlutusV3
          , preambleLicense = Just "MIT"
          }
    , contractValidators =
        Set.fromList
          [ MkValidatorBlueprint
              { validatorTitle = "Web2 authentication minting policy"
              , validatorRedeemer =
                  MkArgumentBlueprint
                    { argumentTitle = Just "Web2Auth"
                    , argumentSchema = definitionRef @Web2Auth
                    , argumentPurpose = Set.singleton Mint
                    , argumentDescription = Nothing
                    }
              , validatorParameters =
                  [ MkParameterBlueprint
                      { parameterTitle = Just "SetupBytes"
                      , parameterSchema = definitionRef @SetupBytes
                      , parameterPurpose = Set.singleton Mint
                      , parameterDescription = Nothing
                      }
                  , MkParameterBlueprint
                      { parameterTitle = Just "Web2Creds"
                      , parameterSchema = definitionRef @Web2Creds
                      , parameterPurpose = Set.singleton Mint
                      , parameterDescription = Nothing
                      }
                  ]
              , validatorDescription = Nothing
              , validatorDatum = Nothing
              , validatorCompiled = Just $ compiledValidator PlutusV3 web2AuthSerialisedScript
              }
          ]
    , contractDefinitions = deriveDefinitions @'[Web2Auth, SetupBytes, Web2Creds]
    }
 where

writeSmartWalletBP :: FilePath -> IO ()
writeSmartWalletBP fp = writeBlueprint fp smartWalletBP

web2AuthSerialisedScript :: ByteString
web2AuthSerialisedScript = serialiseCompiledCode web2AuthCompiledCode & fromShort

web2AuthCompiledCode :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit)
web2AuthCompiledCode = $$(PlutusTx.compile [||untypedWeb2Auth||])
