module ZkFold.Cardano.UPLC.RollupSimple.Compile (
  writeRollupSimpleBP,
  rollupSimpleSerialisedScript,
  rollupSimpleCompiledCode,
) where

import           Data.ByteString                  (ByteString)
import           Data.ByteString.Short            (fromShort)
import           Data.Function                    ((&))
import           Data.Maybe                       (Maybe (..))
import qualified Data.Set                         as Set
import           PlutusLedgerApi.V3
import qualified PlutusTx
import           PlutusTx.Blueprint
import qualified PlutusTx.Prelude                 as PlutusTx
import           Prelude                          (FilePath, IO, ($))

import           ZkFold.Cardano.UPLC.RollupSimple

rollupSimpleBP :: ContractBlueprint
rollupSimpleBP =
  MkContractBlueprint
    { contractId = Just "rollup-simple"
    , contractPreamble =
        MkPreamble
          { preambleTitle = "Rollup Simple"
          , preambleDescription = Just "Validators related to zkFold's Rollup Simple. Visit https://zkfold.io/ for more details"
          , preambleVersion = "0.1.0"
          , preamblePlutusVersion = commonPlutusVersion
          , preambleLicense = Just "MIT"
          }
    , contractValidators =
        Set.fromList
          [ MkValidatorBlueprint
              { validatorTitle = "rollupSimple"
              , validatorRedeemer =
                  MkArgumentBlueprint
                    { argumentTitle = Just "BuiltinData"
                    , argumentSchema = definitionRef @BuiltinData
                    , argumentPurpose = Set.singleton Spend
                    , argumentDescription = Just "No redeemer is required."
                    }
              , validatorParameters =
                  [ MkParameterBlueprint
                      { parameterTitle = Just "ScriptHash"
                      , parameterSchema = definitionRef @ScriptHash
                      , parameterPurpose = Set.singleton Spend
                      , parameterDescription = Nothing
                      }
                  ]
              , validatorDescription = Just "Rollup Simple spend validator"
              , validatorDatum = Nothing -- Omitting information about datum as it is not straightforward.
              , validatorCompiled = Just $ compiledValidator commonPlutusVersion rollupSimpleSerialisedScript
              }
          , MkValidatorBlueprint
              { validatorTitle = "rollupSimpleStake"
              , validatorRedeemer =
                  MkArgumentBlueprint
                    { argumentTitle = Just "RollupSimpleRed"
                    , argumentSchema = definitionRef @RollupSimpleRed
                    , argumentPurpose = Set.singleton Withdraw
                    , argumentDescription = Nothing
                    }
              , validatorParameters =
                  [ MkParameterBlueprint
                      { parameterTitle = Just "RollupConfiguration"
                      , parameterSchema = definitionRef @RollupConfiguration
                      , parameterPurpose = Set.singleton Withdraw
                      , parameterDescription = Nothing
                      }
                  ]
              , validatorDescription = Just "Rollup Simple stake validator"
              , validatorDatum = Nothing
              , validatorCompiled = Just $ compiledValidator commonPlutusVersion rollupSimpleStakeSerialisedScript
              }
          ]
    , contractDefinitions = deriveDefinitions @'[BuiltinData, ScriptHash, RollupSimpleRed, RollupConfiguration]
    }
 where
  commonPlutusVersion = PlutusV3

writeRollupSimpleBP :: FilePath -> IO ()
writeRollupSimpleBP fp = writeBlueprint fp rollupSimpleBP

rollupSimpleSerialisedScript :: ByteString
rollupSimpleSerialisedScript = serialiseCompiledCode rollupSimpleCompiledCode & fromShort

rollupSimpleCompiledCode :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit)
rollupSimpleCompiledCode = $$(PlutusTx.compile [||rollupSimple||])

rollupSimpleStakeSerialisedScript :: ByteString
rollupSimpleStakeSerialisedScript = serialiseCompiledCode rollupSimpleStakeCompiledCode & fromShort

rollupSimpleStakeCompiledCode :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit)
rollupSimpleStakeCompiledCode = $$(PlutusTx.compile [||rollupSimpleStake||])
