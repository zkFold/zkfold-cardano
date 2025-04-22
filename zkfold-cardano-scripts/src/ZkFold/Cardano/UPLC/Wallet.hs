{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.Wallet (
  module ZkFold.Cardano.UPLC.Wallet.Types,
  untypedWeb2Auth,
  untypedCheckSig,
  untypedWallet,
) where

import           PlutusLedgerApi.V1.Value              (valueOf)
import           PlutusLedgerApi.V3
import           PlutusTx.Prelude                      hiding (toList, (*), (+))

import           ZkFold.Algebra.Class                  (MultiplicativeSemigroup (..))
import           ZkFold.Cardano.OnChain.BLS12_381.F    (toInput)
import           ZkFold.Cardano.OnChain.Plonkup        (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data   (SetupBytes)
import           ZkFold.Cardano.UPLC.ForwardingScripts (forwardingReward)
import           ZkFold.Cardano.UPLC.Wallet.Types
import           ZkFold.Protocol.NonInteractiveProof   (NonInteractiveProof (..))

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
      && mintValueMinted txInfoMint  -- Apparently, plinth does not provide convenient (& performant) way to compare 'MintValue' with 'Value'.
      == singleton symb tn 1
      && mintValueBurned txInfoMint
      == mempty
web2Auth _ _ _ = False

{-# INLINEABLE checkSig #-}
checkSig :: CurrencySymbol -> ScriptContext -> Bool
checkSig symb (ScriptContext TxInfo {..} red si) =
  case si of
    CertifyingScript _ _ -> True
    RewardingScript _ ->
      -- extract the value of the i-th output
      let v = txOutValue $ txInfoOutputs !! i
          Signature i j = unsafeFromBuiltinData . getRedeemer $ red
      in -- j-th pubKeyHash is equal to the tokenName of the currency symbol
          valueOf v symb (TokenName $ getPubKeyHash $ txInfoSignatories !! j) > 0
    _anyOther -> False

{-# INLINEABLE wallet #-}
wallet :: ScriptHash -> () -> ScriptContext -> Bool
wallet sh = forwardingReward (getScriptHash sh)

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

{-# INLINEABLE untypedWallet #-}
untypedWallet :: BuiltinData -> BuiltinData -> BuiltinUnit
untypedWallet (unsafeFromBuiltinData -> scriptHash) ctx' =
  let
    ctx = unsafeFromBuiltinData ctx'
    redeemer = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
   in
    check $ wallet scriptHash redeemer ctx
