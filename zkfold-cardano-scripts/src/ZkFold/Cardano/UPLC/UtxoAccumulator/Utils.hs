module ZkFold.Cardano.UPLC.UtxoAccumulator.Utils where

import           Data.Foldable                            (foldr', toList)
import           PlutusLedgerApi.V3                       (Address (..), Credential (..), ToData (..), Value)
import           PlutusTx.Builtins                        (ByteOrder (..), serialiseData)
import           PlutusTx.Prelude                         (BuiltinByteString, Maybe (..), blake2b_224,
                                                           byteStringToInteger, head, ($))
import           Prelude                                  (map, snd, undefined)

import           ZkFold.Algebra.EllipticCurve.BLS12_381   (BLS12_381_G1_Point)
import           ZkFold.Algebra.EllipticCurve.Class       (ScalarFieldOf)
import           ZkFold.Algebra.Field                     (toZp)
import           ZkFold.Cardano.OffChain.BLS12_381        (convertG1, convertZp)
import           ZkFold.Cardano.OffChain.Plonkup          (mkProof, mkSetup)
import           ZkFold.Cardano.OffChain.Utils            (scriptHashOf)
import           ZkFold.Cardano.OnChain.Plonkup.Data      (SetupBytes)
import           ZkFold.Cardano.UPLC.UtxoAccumulator      (M, N, UtxoAccumulatorParameters (..),
                                                           UtxoAccumulatorRedeemer (..), utxoAccumulatorCompiled)
import           ZkFold.Data.Vector                       (init, last)
import           ZkFold.Symbolic.Examples.UtxoAccumulator (accumulationGroupElements, distributionGroupElements,
                                                           switchGroupElement, utxoAccumulatorHash,
                                                           utxoAccumulatorProve, utxoAccumulatorVerifierSetup)

-- TODO: add staking creds
utxoAccumulatorAddress :: UtxoAccumulatorParameters -> Address
utxoAccumulatorAddress par =
  let
      sh = scriptHashOf $ utxoAccumulatorCompiled par
  in
      Address (ScriptCredential sh) undefined

distributionParameters :: Value -> [UtxoAccumulatorParameters]
distributionParameters v =
  let
      lastDistPar :: UtxoAccumulatorParameters
      lastDistPar =
        let
            maybeSwitchAddress = Nothing
            maybeNextAddress   = Nothing
            nextGroupElement   = convertG1 $ last $ distributionGroupElements @N @M
            utxoValue          = v
        in
            UtxoAccumulatorParameters {..}

      prec :: UtxoAccumulatorParameters -> BuiltinByteString -> UtxoAccumulatorParameters
      prec par g = par { maybeNextAddress = Just $ utxoAccumulatorAddress par, nextGroupElement = g }
  in
      foldr'
        (\g acc -> prec (head acc) g : acc)
        [lastDistPar]
        (map convertG1 $ toList $ init $ distributionGroupElements @N @M)

switchParameters :: Value -> UtxoAccumulatorParameters
switchParameters v =
  let
      maybeSwitchAddress = Just $ utxoAccumulatorAddress $ head $ distributionParameters v
      maybeNextAddress   = Nothing
      nextGroupElement   = convertG1 $ switchGroupElement @N @M
      utxoValue          = v
  in
      UtxoAccumulatorParameters {..}

accumulationParameters :: Value -> [UtxoAccumulatorParameters]
accumulationParameters v =
  let
      lastAccPar :: UtxoAccumulatorParameters
      lastAccPar =
        let
            maybeSwitchAddress = Just $ utxoAccumulatorAddress $ switchParameters v
            maybeNextAddress   = Nothing
            nextGroupElement   = convertG1 $ last $ accumulationGroupElements @N @M
            utxoValue          = v
        in
            UtxoAccumulatorParameters {..}

      prec :: UtxoAccumulatorParameters -> BuiltinByteString -> UtxoAccumulatorParameters
      prec par g = par { maybeNextAddress = Just $ utxoAccumulatorAddress par, nextGroupElement = g }
  in
      foldr'
        (\g acc -> prec (head acc) g : acc)
        [lastAccPar]
        (map convertG1 $ toList $ init $ accumulationGroupElements @N @M)

utxoAccumulatorSetupBytesInit :: SetupBytes
utxoAccumulatorSetupBytesInit = mkSetup $ utxoAccumulatorVerifierSetup @N @M

mkAddUtxo ::
     Address
  -> ScalarFieldOf BLS12_381_G1_Point
  -> UtxoAccumulatorRedeemer
mkAddUtxo addr r =
  let
      a = byteStringToInteger BigEndian $ blake2b_224 $ serialiseData $ toBuiltinData addr
  in
      AddUtxo $ convertZp $ utxoAccumulatorHash (toZp a) r

mkRemoveUtxo ::
     [ScalarFieldOf BLS12_381_G1_Point]
  -> [ScalarFieldOf BLS12_381_G1_Point]
  -> Address
  -> ScalarFieldOf BLS12_381_G1_Point
  -> UtxoAccumulatorRedeemer
mkRemoveUtxo hs as addr r =
  let
      a     = byteStringToInteger BigEndian $ blake2b_224 $ serialiseData $ toBuiltinData addr
      proof = mkProof $ snd $ utxoAccumulatorProve @N @M hs as (toZp a) r
  in
      RemoveUtxo addr proof
