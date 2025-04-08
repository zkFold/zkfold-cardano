{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.Wallet where

import           GHC.Generics                             (Generic)
import           PlutusLedgerApi.V1.Value                 (valueOf)
import           PlutusLedgerApi.V3
import           PlutusTx                                 (makeIsDataIndexed)
import           PlutusTx.Prelude                         hiding (toList, (*), (+))
import           Prelude                                  (Show)

import           ZkFold.Base.Algebra.Basic.Class          (MultiplicativeSemigroup (..))
import           ZkFold.Base.Protocol.NonInteractiveProof (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Cardano.OnChain.BLS12_381.F       (toInput)
import           ZkFold.Cardano.OnChain.Plonkup           (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data      (ProofBytes, SetupBytes)
import           ZkFold.Cardano.UPLC.ForwardingScripts    (forwardingReward)

newtype Web2Creds = Web2Creds
  { w2cEmail        :: BuiltinByteString
  } deriving stock (Show, Generic)

makeIsDataIndexed ''Web2Creds [('Web2Creds,0)]

data JWTParts = JWTParts
    { jwtPrefix :: BuiltinByteString
    , jwtSuffix :: BuiltinByteString
    }
  deriving stock (Show, Generic)

makeIsDataIndexed ''JWTParts [('JWTParts,0)]

data Web2Auth = Web2Auth JWTParts ProofBytes TokenName
  deriving stock (Show, Generic)

makeIsDataIndexed ''Web2Auth [('Web2Auth, 0)]

-- TODO: Account for rotation of public keys
-- TODO: Check the client Id
-- TODO: Check the suffix length (must be a predefined size)
-- TODO: Do we need to split bytestrings further due to ledger rules?
{-# INLINABLE web2Auth #-}
-- | Mints tokens paramterized by the user's email and a public key selected by the user.
--
web2Auth :: SetupBytes -> Web2Creds -> ScriptContext -> Bool
web2Auth expModCircuit Web2Creds{..} (ScriptContext TxInfo{..} red (MintingScript symb)) =
  let
      publicInput = toInput (sha2_256 $ jwtPrefix <> w2cEmail <> jwtSuffix) * toInput bs
      Web2Auth JWTParts {..} proof tn@(TokenName bs) = unsafeFromBuiltinData . getRedeemer $ red
  in
      -- Check that the user knows an RSA signature for a JWT containing the email
      verify @PlonkupPlutus @HaskellCore expModCircuit publicInput proof
      -- Check that we mint a token with the correct name
      && txInfoMint == singleton symb tn 1
web2Auth _ _ _ = False

data Signature = Signature Integer Integer
  deriving stock (Show, Generic)

makeIsDataIndexed ''Signature [('Signature, 0)]

{-# INLINABLE checkSig #-}
checkSig :: CurrencySymbol -> ScriptContext -> Bool
checkSig symb (ScriptContext TxInfo{..} red _) =
    -- extract the value of the i-th output
    let v = txOutValue $ txInfoOutputs !! i
        Signature i j = unsafeFromBuiltinData . getRedeemer $ red
    -- j-th pubKeyHash is equal to the tokenName of the currency symbol
    in valueOf v symb (TokenName $ getPubKeyHash $ txInfoSignatories !! j) > 0

{-# INLINABLE wallet #-}
wallet :: ScriptHash -> () -> ScriptContext -> Bool
wallet sh = forwardingReward (getScriptHash sh)

-----------------------------------------------------------

{-# INLINABLE untypedWeb2Auth #-}
untypedWeb2Auth :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
untypedWeb2Auth circuit' creds' ctx' =
  let
    ctx      = unsafeFromBuiltinData ctx'
    circuit :: SetupBytes = unsafeFromBuiltinData circuit'
    creds :: Web2Creds = unsafeFromBuiltinData creds'
  in
    check $ web2Auth circuit creds ctx

{-# INLINABLE untypedCheckSig #-}
untypedCheckSig :: BuiltinData -> BuiltinData -> BuiltinUnit
untypedCheckSig (unsafeFromBuiltinData -> symb) (unsafeFromBuiltinData -> ctx) = check $ checkSig symb ctx

{-# INLINABLE untypedWallet #-}
untypedWallet :: ScriptHash -> BuiltinData -> BuiltinUnit
untypedWallet scriptHash ctx' =
  let
    ctx      = unsafeFromBuiltinData ctx'
    redeemer = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
  in
    check $ wallet scriptHash redeemer ctx
