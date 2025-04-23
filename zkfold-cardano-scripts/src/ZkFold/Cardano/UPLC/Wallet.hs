{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.Wallet (
  module ZkFold.Cardano.UPLC.Wallet.Types,
  untypedWeb2Auth,
  untypedCheckSig,
  wallet,
) where

import           Data.Function                       ((&))
import           PlutusLedgerApi.V1.Value            (valueOf)
import           PlutusLedgerApi.V3
import qualified PlutusTx.AssocMap                   as AssocMap
import qualified PlutusTx.Builtins.Internal          as BI
import           PlutusTx.Prelude                    hiding (toList, (*), (+))

import           ZkFold.Algebra.Class                (MultiplicativeSemigroup (..))
import           ZkFold.Cardano.OnChain.BLS12_381.F  (toInput)
import           ZkFold.Cardano.OnChain.Plonkup      (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data (SetupBytes)
import           ZkFold.Cardano.UPLC.Wallet.Types
import           ZkFold.Protocol.NonInteractiveProof (NonInteractiveProof (..))

-- TODO: Account for rotation of public keys
-- TODO: Check the client Id
-- TODO: Check the suffix length (must be a predefined size)
-- TODO: Do we need to split bytestrings further due to ledger rules?
{-# INLINEABLE web2Auth #-}

-- | Mints tokens paramterized by the user's email and a public key selected by the user.
web2Auth :: SetupBytes -> Web2Creds -> ScriptContext -> Bool
web2Auth expModCircuit Web2Creds {..} (ScriptContext TxInfo {..} red (MintingScript symb)) =
  let
    publicInput = toInput (sha2_256 $ jwtPrefix <> w2cEmail <> jwtSuffix) * toInput bs
    Web2Auth JWTParts {..} proof tn@(TokenName bs) = unsafeFromBuiltinData . getRedeemer $ red
   in
    -- Check that the user knows an RSA signature for a JWT containing the email
    verify @PlonkupPlutus expModCircuit [publicInput] proof
      -- Check that we mint a token with the correct name
      && mintValueMinted txInfoMint -- Apparently, plinth does not provide convenient (& performant) way to compare 'MintValue' with 'Value'.
      == singleton symb tn 1
      && mintValueBurned txInfoMint
      == mempty
      && elem (PubKeyHash bs) txInfoSignatories
web2Auth _ _ _ = False

{-# INLINEABLE checkSig #-}
checkSig :: CurrencySymbol -> ScriptContext -> Bool
checkSig symb (ScriptContext TxInfo {..} red _) =
  -- extract the value of the i-th reference input
  let v = txOutValue $ txInInfoResolved $ txInfoReferenceInputs !! i
      Signature i j = unsafeFromBuiltinData . getRedeemer $ red
   in -- j-th pubKeyHash is equal to the tokenName of the currency symbol
      valueOf v symb (TokenName $ getPubKeyHash $ txInfoSignatories !! j) > 0

{-# INLINEABLE wallet #-}
wallet ::
  -- | Currency symbol of user's minting script.
  BuiltinData ->
  -- | Script hash of stake validator.
  BuiltinData ->
  -- | Script context.
  BuiltinData ->
  BuiltinUnit
wallet (unsafeFromBuiltinData -> cs :: CurrencySymbol) (unsafeFromBuiltinData -> sh :: ScriptHash) sc =
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
         in AssocMap.member (toBuiltinData cs) txInfoMint
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

-----------------------------------------------------------

{-# INLINEABLE untypedWeb2Auth #-}
untypedWeb2Auth :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
untypedWeb2Auth circuit' creds' ctx' =
  let
    ctx = unsafeFromBuiltinData ctx'
    circuit :: SetupBytes = unsafeFromBuiltinData circuit'
    creds :: Web2Creds = unsafeFromBuiltinData creds'
   in
    check $ web2Auth circuit creds ctx

{-# INLINEABLE untypedCheckSig #-}
untypedCheckSig :: BuiltinData -> BuiltinData -> BuiltinUnit
untypedCheckSig (unsafeFromBuiltinData -> symb) (unsafeFromBuiltinData -> ctx) = check $ checkSig symb ctx
