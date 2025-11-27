
module ZkFold.Cardano.UPLC.RollupSimple.Compile (
  writeRollupSimpleBP,
  rollupSimpleSerialisedScript,
  rollupSimpleCompiledCode,
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

import           ZkFold.Cardano.OnChain.Plonkup.Data (SetupBytes)
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
                    { argumentTitle = Just "RollupSimpleRed"
                    , argumentSchema = definitionRef @RollupSimpleRed
                    , argumentPurpose = Set.singleton Spend
                    , argumentDescription = Nothing
                    }
              , validatorParameters =
                  [ MkParameterBlueprint
                      { parameterTitle = Just "SetupBytes"
                      , parameterSchema = definitionRef @SetupBytes
                      , parameterPurpose = Set.singleton Spend
                      , parameterDescription = Nothing
                      }
                  , MkParameterBlueprint
                      { parameterTitle = Just "CurrencySymbol"
                      , parameterSchema = definitionRef @CurrencySymbol
                      , parameterPurpose = Set.singleton Spend
                      , parameterDescription = Nothing
                      }
                  , MkParameterBlueprint
                      { parameterTitle = Just "TokenName"
                      , parameterSchema = definitionRef @TokenName
                      , parameterPurpose = Set.singleton Spend
                      , parameterDescription = Nothing
                      }
                  ]
              , validatorDescription = Just "Rollup Simple validator"
              , validatorDatum = Nothing
              , validatorCompiled = Just $ compiledValidator commonPlutusVersion rollupSimpleSerialisedScript
              }
          ]
    , contractDefinitions = deriveDefinitions @'[SetupBytes, CurrencySymbol, TokenName, RollupState, RollupSimpleRed] }
 where
  commonPlutusVersion = PlutusV3

writeRollupSimpleBP :: FilePath -> IO ()
writeRollupSimpleBP fp = writeBlueprint fp rollupSimpleBP

rollupSimpleSerialisedScript :: ByteString
rollupSimpleSerialisedScript = serialiseCompiledCode rollupSimpleCompiledCode & fromShort

rollupSimpleCompiledCode :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit)
rollupSimpleCompiledCode = $$(PlutusTx.compile [||rollupSimple||])
