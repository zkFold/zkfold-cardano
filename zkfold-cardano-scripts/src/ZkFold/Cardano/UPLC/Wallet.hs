{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.Wallet (
  module ZkFold.Cardano.UPLC.Wallet.Types,
  web2Auth,
  checkSig,
  wallet,
) where

import           Data.Function                       ((&))
import           PlutusLedgerApi.V1.Value            (valueOf)
import           PlutusLedgerApi.V3
import qualified PlutusTx.AssocMap                   as AssocMap
import qualified PlutusTx.Builtins.Internal          as BI
import           PlutusTx.Prelude                    hiding (toList, (*), (+))

import           ZkFold.Cardano.OnChain.BLS12_381.F  (toInput)
import           ZkFold.Cardano.OnChain.Plonkup      (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data (SetupBytes)
import           ZkFold.Cardano.UPLC.Wallet.Internal (base64urlEncode)
import           ZkFold.Cardano.UPLC.Wallet.Types
import           ZkFold.Protocol.NonInteractiveProof (NonInteractiveProof (..))


{-# INLINEABLE web2Auth #-}

-- | Mints tokens paramterized by the user's email and a public key selected by the user.
web2Auth ::
  -- | Wallet config
  BuiltinData ->
  -- | 'ScriptContext'.
  BuiltinData ->
  BuiltinUnit
web2Auth (unsafeFromBuiltinData -> OnChainWalletConfig {..}) sc =
  check
    $ let
        encodedJwt = base64urlEncode jwtHeader <> "." <> base64urlEncode (jwtPrefix <> ocwcUidPrefix <> ocwcUid <> jwtSuffix)
        jwtHash = sha2_256 encodedJwt
        publicInput = [toInput jwtHash, toInput bs]
       in
        -- Check that the user knows an RSA signature for a JWT containing the email
         verify @PlonkupPlutus expModCircuit publicInput proof
          -- Check that we mint a token with the correct name
          && AssocMap.lookup (toBuiltinData symb) txInfoMint
          == Just (toBuiltinData $ AssocMap.singleton tn (1 :: Integer))
          && elem (PubKeyHash bs) txInfoSignatories
--          && hasZkFoldFee
 where
  -- tx reference inputs
  refInput = txInfo & BI.tail & BI.head & BI.unsafeDataAsList & BI.head -- TxInInfo
  refInputResolved = refInput & BI.unsafeDataAsConstr & BI.snd & BI.tail & BI.head -- TxOut
  txOutL = refInputResolved & BI.unsafeDataAsConstr & BI.snd & BI.tail
  txValue = txOutL & BI.head & unsafeFromBuiltinData

  -- find beacon datum
  beaconDatum =
      if   valueOf txValue ocwcBeaconPolicyId ocwcBeaconName == 0
      then traceError "No beacon token" -- error ()
      else txOutL & BI.tail & BI.head & unsafeFromBuiltinData

  -- decode beacon datum
  setupBytesMap =
      case beaconDatum of
        OutputDatum datum -> unsafeFromBuiltinData $ getDatum datum
        _                 -> error ()

  Just setupBytes = AssocMap.lookup (toBuiltinData kid) setupBytesMap

  expModCircuit :: SetupBytes
  expModCircuit = unsafeFromBuiltinData setupBytes

  txInfoL = BI.unsafeDataAsConstr sc & BI.snd
  txInfo = txInfoL & BI.head & BI.unsafeDataAsConstr & BI.snd
  redL = txInfoL & BI.tail
  Web2Auth JWTParts {..} proof tn@(TokenName bs) (KeyId kid) = redL & BI.head & unsafeFromBuiltinData
  (MintingScript symb) = redL & BI.tail & BI.head & unsafeFromBuiltinData
  txInfoMintL =
    txInfo
      & BI.tail
      & BI.tail
      & BI.tail
      & BI.tail
  txInfoMint :: Map BuiltinData BuiltinData =
    txInfoMintL
      & BI.head
      & unsafeFromBuiltinData
  txInfoSignatories :: [PubKeyHash] =
    txInfoMintL
      & BI.tail
      & BI.tail
      & BI.tail
      & BI.tail
      & BI.head
      & unsafeFromBuiltinData
          {--
  adaFee :: Value
  adaFee = singleton adaSymbol adaToken ocwcFee

  txInfoOutputsL =
    txInfo
      & BI.tail
      & BI.tail
  txInfoOutputs :: [TxOut]
  txInfoOutputs = txInfoOutputsL & BI.head & unsafeFromBuiltinData

  hasZkFoldFee = any (\(TxOut addr val _ _) -> addr == ocwcFeeAddress && val == adaFee) txInfoOutputs
--}

{-# INLINEABLE checkSig #-}
checkSig ::
  -- | 'CurrencySymbol'.
  BuiltinData ->
  -- | 'ScriptContext'.
  BuiltinData ->
  BuiltinUnit
checkSig (unsafeFromBuiltinData -> (symb :: CurrencySymbol)) sc =
  check
    $
    -- extract the value of the i-th output
    let v = txOutValue $ txInfoOutputs !! i
     in -- j-th pubKeyHash is equal to the tokenName of the currency symbol
        valueOf v symb (TokenName $ getPubKeyHash $ txInfoSignatories !! j) > 0
 where
  txInfoL = BI.unsafeDataAsConstr sc & BI.snd
  txInfo = txInfoL & BI.head & BI.unsafeDataAsConstr & BI.snd
  Signature i j = txInfoL & BI.tail & BI.head & unsafeFromBuiltinData
  txInfoOutputsL =
    txInfo
      & BI.tail
      & BI.tail
  txInfoOutputs = txInfoOutputsL & BI.head & unsafeFromBuiltinData
  txInfoSignatories :: [PubKeyHash] =
    txInfoOutputsL
      & BI.tail
      & BI.tail
      & BI.tail
      & BI.tail
      & BI.tail
      & BI.tail
      & BI.head
      & unsafeFromBuiltinData

{-# INLINEABLE wallet #-}
wallet ::
  -- | Currency symbol of user's minting script.
  BuiltinData ->
  -- | Script hash of stake validator.
  BuiltinData ->
  -- | Script context.
  BuiltinData ->
  BuiltinUnit
wallet cs (unsafeFromBuiltinData -> sh :: ScriptHash) sc =
  check
    $ if red == 0
      then
        -- We require the minting script.
        let txInfoMint :: Map BuiltinData BuiltinData =
              txInfo
                & BI.tail
                & BI.tail
                & BI.tail
                & BI.tail
                & BI.head
                & unsafeFromBuiltinData
         in AssocMap.member cs txInfoMint
      -- We require the withdrawal script.
      else
        (red == 1)
          && ( let txInfoWrdl :: Map BuiltinData BuiltinData =
                    txInfo
                      & BI.tail
                      & BI.tail
                      & BI.tail
                      & BI.tail
                      & BI.tail
                      & BI.tail
                      & BI.head
                      & unsafeFromBuiltinData
                in AssocMap.member (toBuiltinData $ ScriptCredential sh) txInfoWrdl
             )
 where
  txInfoL = BI.unsafeDataAsConstr sc & BI.snd
  txInfo = txInfoL & BI.head & BI.unsafeDataAsConstr & BI.snd
  -- Note that 'BuiltinInteger' is a type synonym for 'Integer' so there is no extra cost here.
  red :: Integer = txInfoL & BI.tail & BI.head & unsafeFromBuiltinData
