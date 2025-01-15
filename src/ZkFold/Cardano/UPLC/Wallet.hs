{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ZkFold.Cardano.UPLC.Wallet where

import           GHC.Generics                             (Generic)
import           PlutusLedgerApi.V1.Value                 (lovelaceValue)
import           PlutusLedgerApi.V3
import           PlutusTx                                 (makeIsDataIndexed, makeLift)
import qualified PlutusTx.AssocMap                        as M
import           PlutusTx.Prelude                         hiding (toList, (*), (+))
import           Prelude                                  (Show)

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
    { wrTxDate :: BuiltinByteString
    , wrCreds  :: SpendingCreds
    }
  deriving stock (Show, Generic)

makeLift ''WalletRedeemer
makeIsDataIndexed ''WalletRedeemer [('WalletRedeemer, 0)]

{-# INLINABLE wallet #-}
wallet :: SetupBytes -> WalletSetup -> WalletRedeemer -> ScriptContext -> Bool
wallet zkpCheck WalletSetup{..} WalletRedeemer{..} ctx@(ScriptContext TxInfo{..} _ scriptInfo) =
    case scriptInfo of
        SpendingScript _ _ -> forwardingReward (getTxId $ txInfoId) () ctx
        _                  -> case wrCreds of
              SpendWithSignature sign -> any (== sign) $ getPubKeyHash <$> txInfoSignatories
              SpendWithWeb2Token w2c  -> zkpPasses w2c && outputsCorrect
    where
        compressedPI Web2Creds{..} = toInput . blake2b_224 $ wUserId `appendByteString` wTokenHash `appendByteString` (fromInput . toF $ wAmount)
        zkpPasses w2c = verify @PlonkupPlutus @HaskellCore zkpCheck (compressedPI w2c)

        outputsCorrect = and
            [ length txInfoOutputs == 2                 -- only two outputs
            , all (null . txOutDatumHash) txInfoOutputs -- datums are empty
            , lovelaceValue (Lovelace wAmount) == txOutValue (head txInfoOutputs) -- the amount is correct
            ]


{-# INLINABLE untypedWallet #-}
untypedWallet :: SetupBytes -> WalletSetup -> BuiltinData -> BuiltinUnit
untypedWallet zkpCheck setup ctx' =
  let
    ctx      = unsafeFromBuiltinData ctx'
    redeemer = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
  in
    check $ wallet zkpCheck setup redeemer ctx
