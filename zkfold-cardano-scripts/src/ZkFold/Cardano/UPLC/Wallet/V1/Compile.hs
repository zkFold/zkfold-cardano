module ZkFold.Cardano.UPLC.Wallet.V1.Compile (
  writeSmartWalletBP,
  walletSerialisedScript,
  walletCompiledCode,
  rewardingZKPSerialisedScript,
  rewardingZKPCompiledCode,
) where

import           Data.ByteString                     (ByteString)
import           Data.ByteString.Short               (fromShort)
import           Data.Function                       ((&))
import           Data.Maybe                          (Maybe (..))
import qualified Data.Set                            as Set
import           PlutusLedgerApi.V3
import qualified PlutusTx
import           PlutusTx.Blueprint
import qualified PlutusTx.Prelude                    as PlutusTx 
import           Prelude                             (FilePath, IO, ($))

import           ZkFold.Cardano.UPLC.Wallet.V1
import           ZkFold.Cardano.UPLC.Wallet.V1.Types

smartWalletBP :: ContractBlueprint
smartWalletBP =
  MkContractBlueprint
    { contractId = Just "smart-wallet-v1"
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
                      { parameterTitle = Just "Dummy parameter"
                      , parameterSchema = definitionRef @PlutusTx.Integer
                      , parameterPurpose = Set.singleton Spend 
                      , parameterDescription = Nothing
                      }
                  , MkParameterBlueprint
                      { parameterTitle = Just "Email"
                      , parameterSchema = definitionRef @PlutusTx.BuiltinByteString
                      , parameterPurpose = Set.singleton Spend 
                      , parameterDescription = Nothing
                      }
                  , MkParameterBlueprint
                      { parameterTitle = Just "Script hash"
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
              { validatorTitle = "rewardingZKP"
              , validatorRedeemer =
                  MkArgumentBlueprint
                    { argumentTitle = Just "Rewarding script redeemer"
                    , argumentSchema = definitionRef @RewardingRedeemer
                    , argumentPurpose = Set.singleton Withdraw
                    , argumentDescription = Nothing
                    }
              , validatorParameters =
                  [ MkParameterBlueprint
                      { parameterTitle = Just "Wallet config"
                      , parameterSchema = definitionRef @OnChainWalletConfig
                      , parameterPurpose = Set.singleton Withdraw
                      , parameterDescription = Nothing
                      }
                  ]
              , validatorDescription = Just "Smart wallet rewards script"
              , validatorDatum = Nothing
              , validatorCompiled = Just $ compiledValidator commonPlutusVersion rewardingZKPSerialisedScript
              }
          ]
    , contractDefinitions = deriveDefinitions @'[PlutusTx.Integer, RewardingRedeemer, OnChainWalletConfig, PlutusTx.BuiltinByteString, (), ScriptHash, PlutusTx.BuiltinData, CurrencySymbol]
    }
 where
  commonPlutusVersion = PlutusV3

writeSmartWalletBP :: FilePath -> IO ()
writeSmartWalletBP fp = writeBlueprint fp smartWalletBP

walletSerialisedScript :: ByteString
walletSerialisedScript = serialiseCompiledCode walletCompiledCode & fromShort

walletCompiledCode :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit)
walletCompiledCode = $$(PlutusTx.compile [||wallet||])

rewardingZKPSerialisedScript :: ByteString
rewardingZKPSerialisedScript = serialiseCompiledCode rewardingZKPCompiledCode & fromShort

rewardingZKPCompiledCode :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit)
rewardingZKPCompiledCode = $$(PlutusTx.compile [||rewardingZKP||])

