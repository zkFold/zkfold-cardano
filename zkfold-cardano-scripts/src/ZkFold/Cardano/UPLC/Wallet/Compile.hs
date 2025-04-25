module ZkFold.Cardano.UPLC.Wallet.Compile (
  writeSmartWalletBP,
  web2AuthSerialisedScript,
  web2AuthCompiledCode,
  walletSerialisedScript,
  walletCompiledCode,
  checkSigSerialisedScript,
  checkSigCompiledCode,
) where

import           Data.ByteString                     (ByteString)
import           Data.ByteString.Short               (fromShort)
import           Data.Function                       ((&))
import           Data.Maybe                          (Maybe (..))
import qualified Data.Set                            as Set
import           PlutusLedgerApi.V3
import qualified PlutusTx
import           PlutusTx.Blueprint
import qualified PlutusTx.Prelude                    as PlutusTx (BuiltinUnit)
import           Prelude                             (FilePath, IO, ($))

import           ZkFold.Cardano.OnChain.Plonkup.Data (SetupBytes)
import           ZkFold.Cardano.UPLC.Wallet

smartWalletBP :: ContractBlueprint
smartWalletBP =
  MkContractBlueprint
    { contractId = Just "smart-wallet"
    , contractPreamble =
        MkPreamble
          { preambleTitle = "Smart Wallet"
          , preambleDescription = Just "Validators related to zkFold's Smart Wallet. Visit https://zkfold.io/ for more details"
          , preambleVersion = "0.1.0"
          , preamblePlutusVersion = commonPlutusVersion
          , preambleLicense = Just "MIT"
          }
    , contractValidators =
        Set.fromList
          [ MkValidatorBlueprint
              { validatorTitle = "web2Auth"
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
              , validatorDescription = Just "Smart wallet web2 authentication minting policy"
              , validatorDatum = Nothing
              , validatorCompiled = Just $ compiledValidator commonPlutusVersion web2AuthSerialisedScript
              }
          , MkValidatorBlueprint
              { validatorTitle = "wallet"
              , validatorRedeemer =
                  MkArgumentBlueprint
                    { argumentTitle = Just "Unit"
                    , argumentSchema = definitionRef @()
                    , argumentPurpose = Set.singleton Spend
                    , argumentDescription = Nothing
                    }
              , validatorParameters =
                  [ MkParameterBlueprint
                      { parameterTitle = Just "ScriptHash"
                      , parameterSchema = definitionRef @ScriptHash
                      , parameterPurpose = Set.singleton Spend
                      , parameterDescription = Nothing
                      }
                  ]
              , validatorDescription = Just "Smart wallet spending validator"
              , validatorDatum =
                  Just $
                    MkArgumentBlueprint
                      { argumentTitle = Nothing
                      , argumentSchema = definitionRef @PlutusTx.BuiltinData
                      , argumentPurpose = Set.singleton Spend
                      , argumentDescription = Nothing
                      }
              , validatorCompiled = Just $ compiledValidator commonPlutusVersion walletSerialisedScript
              }
          , MkValidatorBlueprint
              { validatorTitle = "checkSig"
              , validatorRedeemer =
                  MkArgumentBlueprint
                    { argumentTitle = Just "Signature"
                    , argumentSchema = definitionRef @Signature
                    , argumentPurpose = Set.singleton Withdraw
                    , argumentDescription = Nothing
                    }
              , validatorParameters =
                  [ MkParameterBlueprint
                      { parameterTitle = Just "CurrencySymbol"
                      , parameterSchema = definitionRef @CurrencySymbol
                      , parameterPurpose = Set.singleton Withdraw
                      , parameterDescription = Nothing
                      }
                  ]
              , validatorDescription = Just "Smart wallet rewards script"
              , validatorDatum = Nothing
              , validatorCompiled = Just $ compiledValidator commonPlutusVersion checkSigSerialisedScript
              }
          ]
    , contractDefinitions = deriveDefinitions @'[Web2Auth, SetupBytes, Web2Creds, (), ScriptHash, PlutusTx.BuiltinData, Signature, CurrencySymbol]
    }
 where
  commonPlutusVersion = PlutusV3

writeSmartWalletBP :: FilePath -> IO ()
writeSmartWalletBP fp = writeBlueprint fp smartWalletBP

web2AuthSerialisedScript :: ByteString
web2AuthSerialisedScript = serialiseCompiledCode web2AuthCompiledCode & fromShort

web2AuthCompiledCode :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit)
web2AuthCompiledCode = $$(PlutusTx.compile [||web2Auth||])

walletSerialisedScript :: ByteString
walletSerialisedScript = serialiseCompiledCode walletCompiledCode & fromShort

walletCompiledCode :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit)
walletCompiledCode = $$(PlutusTx.compile [||wallet||])

checkSigSerialisedScript :: ByteString
checkSigSerialisedScript = serialiseCompiledCode checkSigCompiledCode & fromShort

checkSigCompiledCode :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit)
checkSigCompiledCode = $$(PlutusTx.compile [||untypedCheckSig||])
