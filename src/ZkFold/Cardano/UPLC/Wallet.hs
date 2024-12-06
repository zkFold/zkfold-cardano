{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ZkFold.Cardano.UPLC.Wallet where

import           GHC.Generics                     (Generic)
import           PlutusLedgerApi.V3
import           PlutusTx                         (makeIsDataIndexed, makeLift)
import           PlutusTx.Prelude                 hiding (toList, (*), (+))
import           Prelude                          (Show, undefined)

import           ZkFold.Cardano.OnChain.BLS12_381 (F)

data WalletSetup = WalletSetup
  { wsPubKeyHash :: BuiltinByteString
  , wsWeb2UserId :: BuiltinByteString
  } deriving stock (Show, Generic)

makeLift ''WalletSetup
makeIsDataIndexed ''WalletSetup [('WalletSetup,0)]

data SpendingCreds = SpendWithSignature BuiltinByteString | SpendWithWeb2Token BuiltinByteString
  deriving stock (Show, Generic)

makeIsDataIndexed ''SpendingCreds [('SpendWithSignature,0),('SpendWithWeb2Token,1)]

data WalletRedeemer = WalletRedeemer
    { wrTxDate :: BuiltinByteString
    , wrCreds  :: SpendingCreds
    }
  deriving stock (Show, Generic)

makeLift ''WalletRedeemer
makeIsDataIndexed ''WalletRedeemer [('WalletRedeemer, 0)]

type Web2Check =
       BuiltinByteString -- ^ token
    -> BuiltinByteString -- ^ date (64 bits)
    -> BuiltinByteString -- ^ client id
    -> Bool

{-# INLINABLE wallet #-}
wallet :: Web2Check -> WalletSetup -> WalletRedeemer -> ScriptContext -> Bool
wallet zkpCheck WalletSetup{..} WalletRedeemer{..} ctx@(ScriptContext TxInfo{..} _ scriptInfo) =
    signedCorrectly && valueIsCorrect && fwd
    where
        signedCorrectly =
            case wrCreds of
              SpendWithSignature sign  -> verifyEd25519Signature wsPubKeyHash undefined sign
              SpendWithWeb2Token token -> zkpCheck token wrTxDate wsWeb2UserId

        valueIsCorrect = sumInputs - sumOutputs - txInfoFee >= 0

        sumTx :: [TxOut] -> Lovelace
        sumTx = sum . fmap txOutValue

        sumInputs = sumTx . fmap txInInfoResolved $ txInfoInputs

        sumOutputs = sumTx txInfoOutputs

        fwd =
            case scriptInfo of
              Spending _ _ -> forwardingReward undefined () ctx
              _            -> True



{-# INLINABLE untypedWallet #-}
untypedWallet :: Web2Check -> WalletSetup -> BuiltinData -> BuiltinUnit
untypedWallet zkpCheck setup ctx' =
  let
    ctx      = unsafeFromBuiltinData ctx'
    redeemer = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
  in
    check $ wallet zkpCheck setup redeemer ctx
