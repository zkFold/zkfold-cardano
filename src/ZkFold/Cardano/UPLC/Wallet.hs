{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ZkFold.Cardano.UPLC.Wallet where

import           GHC.Generics                             (Generic)
import           PlutusLedgerApi.V1.Value                 (lovelaceValue)
import           PlutusLedgerApi.V3
import           PlutusLedgerApi.V3.Contexts              (findOwnInput)
import           PlutusTx                                 (makeIsDataIndexed, makeLift)
import           PlutusTx.Prelude                         hiding (toList, (*), (+))
import           Prelude                                  (Show, foldl1)

import           ZkFold.Base.Protocol.NonInteractiveProof (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Cardano.OnChain.BLS12_381.F       (fromInput, toF, toInput)
import           ZkFold.Cardano.OnChain.Plonkup           (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data      (ProofBytes, SetupBytes)
import           ZkFold.Cardano.UPLC.ForwardingScripts    (forwardingReward)

data WalletSetup = WalletSetup
  { wsPubKeyHash :: BuiltinByteString
  , wsWeb2UserId :: BuiltinByteString
  } deriving stock (Show, Generic)

makeLift ''WalletSetup
makeIsDataIndexed ''WalletSetup [('WalletSetup,0)]

data Web2Creds = Web2Creds
    { wUserId    :: BuiltinByteString
    , wTokenHash :: BuiltinByteString
    , wAmount    :: Integer
    }
  deriving stock (Show, Generic)

makeLift ''Web2Creds
makeIsDataIndexed ''Web2Creds [('Web2Creds,0)]

data SpendingCreds = SpendWithSignature BuiltinByteString | SpendWithWeb2Token Web2Creds
  deriving stock (Show, Generic)

makeLift ''SpendingCreds
makeIsDataIndexed ''SpendingCreds [('SpendWithSignature,0),('SpendWithWeb2Token,1)]

data WalletRedeemer = WalletRedeemer
    { wrTxDate      :: BuiltinByteString
    , wrTxRecipient :: BuiltinByteString
    , wrZkp         :: ProofBytes
    , wrCreds       :: SpendingCreds
    }
  deriving stock (Show, Generic)

makeLift ''WalletRedeemer
makeIsDataIndexed ''WalletRedeemer [('WalletRedeemer, 0)]

{-# INLINABLE wallet #-}
-- | This script verifies that a transaction was either signed with a signature or has a ZKP associated with it allowing to access the funds.
-- If the script purpose is Spending, it forwards the verification to the corresponding contract
--
wallet :: SetupBytes -> WalletSetup -> WalletRedeemer -> ScriptContext -> Bool
wallet zkpCheck WalletSetup{..} WalletRedeemer{..} ctx@(ScriptContext TxInfo{..} _ scriptInfo) =
    case (scriptInfo, maybeScriptHash) of
      (SpendingScript _ _, Just scriptHash) -> forwardingReward scriptHash () ctx
      (SpendingScript _ _, _)               -> False
      _                                     -> case wrCreds of
            SpendWithSignature sign -> any (== sign) $ getPubKeyHash <$> txInfoSignatories -- pubKayHash is present in the signatories list
            SpendWithWeb2Token w2c  -> zkpPasses w2c && outputsCorrect w2c
    where
        maybeScriptHash = do
            inp <- findOwnInput ctx
            let addrCreds = addressCredential . txOutAddress . txInInfoResolved $ inp
            case addrCreds of
              ScriptCredential scr -> pure $ getScriptHash scr
              _                    -> Nothing

        compressedPI Web2Creds{..} = toInput . blake2b_224 $ foldl1 appendByteString [wUserId, wTokenHash, fromInput . toF $ wAmount, wrTxRecipient]
        zkpPasses w2c = verify @PlonkupPlutus @HaskellCore zkpCheck (compressedPI w2c) wrZkp

        outputsCorrect Web2Creds {..} = and
            [ length txInfoOutputs == 2                           -- only two outputs
            , all ((== NoOutputDatum) . txOutDatum) txInfoOutputs -- datums are empty
            , lovelaceValue (Lovelace wAmount) == txOutValue (head txInfoOutputs)  -- the amount is correct
            , wrTxRecipient == (getCredential . txOutAddress) (head txInfoOutputs) -- the recipient's address is correct
            , wsPubKeyHash == (getCredential . txOutAddress) (txInfoOutputs !! 1)  -- the wallet's change address is correct
            ]

        getCredential :: Address -> BuiltinByteString
        getCredential (Address (PubKeyCredential pkey) _) = getPubKeyHash pkey
        getCredential (Address (ScriptCredential scr)  _) = getScriptHash scr


{-# INLINABLE untypedWallet #-}
untypedWallet :: SetupBytes -> WalletSetup -> BuiltinData -> BuiltinUnit
untypedWallet zkpCheck setup ctx' =
  let
    ctx      = unsafeFromBuiltinData ctx'
    redeemer = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
  in
    check $ wallet zkpCheck setup redeemer ctx
