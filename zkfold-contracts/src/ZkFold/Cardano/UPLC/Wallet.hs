{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.Wallet where

import           GHC.Generics                     (Generic)
import           PlutusLedgerApi.V3               (BuiltinByteString, BuiltinData, Redeemer (..), ScriptContext (..),
                                                   UnsafeFromData (..))
import           PlutusTx                         (makeIsDataIndexed, makeLift)
import           PlutusTx.Prelude                 (Bool, BuiltinUnit, check, ($), (.))
import           Prelude                          (Show, undefined)

import           ZkFold.Cardano.OnChain.BLS12_381 (F)

data WalletSetup = WalletSetup
  { wsPubKeyHash :: BuiltinByteString
  , wsWeb2UserId :: F
  } deriving stock (Show, Generic)

makeLift ''WalletSetup
makeIsDataIndexed ''WalletSetup [('WalletSetup,0)]

data WalletRedeemer = SpendWithSignature BuiltinByteString | SpendWithWeb2Token F
  deriving stock (Show, Generic)

makeIsDataIndexed ''WalletRedeemer [('SpendWithSignature,0),('SpendWithWeb2Token,1)]

-- TODO: Implement the smart wallet script.
{-# INLINABLE wallet #-}
wallet :: WalletSetup -> WalletRedeemer -> ScriptContext -> Bool
wallet = undefined

{-# INLINABLE untypedWallet #-}
untypedWallet :: WalletSetup -> BuiltinData -> BuiltinUnit
untypedWallet setup ctx' =
  let
    ctx      = unsafeFromBuiltinData ctx'
    redeemer = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
  in
    check $ wallet setup redeemer ctx
