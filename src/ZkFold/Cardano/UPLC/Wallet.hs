{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ZkFold.Cardano.UPLC.Wallet where

import           GHC.Generics                          (Generic)
import           PlutusLedgerApi.V1.Value              (lovelaceValue)
import           PlutusLedgerApi.V3
import           PlutusTx                              (makeIsDataIndexed, makeLift)
import qualified PlutusTx.AssocMap                     as M
import           PlutusTx.Prelude                      hiding (toList, (*), (+))
import           Prelude                               (Show)

import           ZkFold.Cardano.UPLC.ForwardingScripts (forwardingReward)

data WalletSetup = WalletSetup
  { wsPubKeyHash :: BuiltinByteString
  , wsWeb2UserId :: BuiltinByteString
  } deriving stock (Show, Generic)

makeLift ''WalletSetup
makeIsDataIndexed ''WalletSetup [('WalletSetup,0)]

data SpendingCreds = SpendWithSignature BuiltinByteString | SpendWithWeb2Token BuiltinByteString
  deriving stock (Show, Generic)

makeLift ''SpendingCreds
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
              SpendWithSignature sign  -> verifyEd25519Signature wsPubKeyHash (getTxId $ txInfoId) sign
              SpendWithWeb2Token token -> zkpCheck token wrTxDate wsWeb2UserId

        valueIsCorrect = M.all (M.all (>= 0)) $ getValue $ sumInputs - sumOutputs - feeValue

        feeValue :: Value
        feeValue = lovelaceValue txInfoFee

        sumTx :: [TxOut] -> Value
        sumTx = mconcat . fmap txOutValue

        sumInputs = sumTx . fmap txInInfoResolved $ txInfoInputs

        sumOutputs = sumTx txInfoOutputs

        fwd =
            case scriptInfo of
              SpendingScript _ _ -> forwardingReward (getTxId $ txInfoId) () ctx
              _                  -> True


{-# INLINABLE untypedWallet #-}
untypedWallet :: Web2Check -> WalletSetup -> BuiltinData -> BuiltinUnit
untypedWallet zkpCheck setup ctx' =
  let
    ctx      = unsafeFromBuiltinData ctx'
    redeemer = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
  in
    check $ wallet zkpCheck setup redeemer ctx
