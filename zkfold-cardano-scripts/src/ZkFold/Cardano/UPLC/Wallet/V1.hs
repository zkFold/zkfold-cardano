{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.Wallet.V1 (
  rewardingZKP,
  wallet,
) where

import           Data.Function                       ((&))
import           PlutusLedgerApi.V1.Value            (valueOf)
import           PlutusLedgerApi.V3
import qualified PlutusTx.AssocMap                   as AssocMap
import           PlutusTx.Builtins
import qualified PlutusTx.Builtins.Internal          as BI
import           PlutusTx.Prelude
import           PlutusTx.Show                       (show)

import           ZkFold.Cardano.UPLC.Wallet.Internal (base64urlEncode)
import           ZkFold.Cardano.UPLC.Wallet.V1.Types

{-# INLINEABLE pad  #-}

-- | As per RFC 3447
-- https://datatracker.ietf.org/doc/html/rfc3447#section-9.2
--
-- In RSA, message hash is padded to 2048 bits with this value
--
pad :: Integer
pad = 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff003031300d060960864801650304020105000420000000000000000000000000000000000000000000000000000000000000000000

{-# INLINEABLE rewardingZKP #-}

-- | Verifies that the JWT is properly signed
rewardingZKP ::
  -- | Wallet config
  BuiltinData ->
  -- | 'ScriptContext'.
  BuiltinData ->
  BuiltinUnit
rewardingZKP (unsafeFromBuiltinData -> OnChainWalletConfig {..}) sc =
  check
    $ let
        encodedJwt = base64urlEncode jwtHeader <> "." <> base64urlEncode (jwtPrefix <> ocwcUidPrefix <> userId <> jwtSuffix)
        jwtHash = sha2_256 encodedJwt
        paddedHash = pad + byteStringToInteger BigEndian jwtHash

        c = integerToByteString BigEndian 2048 paddedHash

        correctLengths = length v == 16 && length aut == 16

        verified = and $ flip map (zip v aut) $ \(vi, auti) ->
            let i = (byteStringToInteger BigEndian $ sha2_256 (encodeUtf8 $ show c <> show aut)) `modulo` pubE
                lhs = expMod vi pubE pubN
                rhs = (auti * expMod paddedHash i pubN) `modulo` pubN
             in lhs == rhs
       in
        -- Check that the user knows an RSA signature for a JWT containing the email
         correctLengths && verified && hasZkFoldFee
 where
  -- tx reference inputs
  refInput = txInfo & BI.tail & BI.head & BI.unsafeDataAsList & BI.head -- TxInInfo
  refInputResolved = refInput & BI.unsafeDataAsConstr & BI.snd & BI.tail & BI.head -- TxOut
  txOutL = refInputResolved & BI.unsafeDataAsConstr & BI.snd & BI.tail

  -- find beacon datum
  beaconDatum = txOutL & BI.tail & BI.head & unsafeFromBuiltinData

  -- decode beacon datum
  pubkeyMap =
      case beaconDatum of
        OutputDatum datum -> unsafeFromBuiltinData $ getDatum datum
        _                 -> error ()

  Just PubKey {..} = AssocMap.lookup (toBuiltinData kid) pubkeyMap

  txInfoL = BI.unsafeDataAsConstr sc & BI.snd
  txInfo = txInfoL & BI.head & BI.unsafeDataAsConstr & BI.snd
  redL = txInfoL & BI.tail
  RewardingRedeemer JWTParts {..} UserId {..} SigmaProof {..} (KeyId kid) = redL & BI.head & unsafeFromBuiltinData

  txInfoOutputsL =
    txInfo
      & BI.tail
      & BI.tail
  txInfoOutputs :: [TxOut]
  txInfoOutputs = txInfoOutputsL & BI.head & unsafeFromBuiltinData

  hasZkFoldFee = any (\(TxOut addr val _ _) -> addr == ocwcFeeAddress && valueOf val adaSymbol adaToken >= ocwcFee) txInfoOutputs


{-# INLINEABLE wallet #-}
wallet ::
  -- | Dummy parameter for extra addresses
  BuiltinData ->
  -- | Currency symbol of user's minting script.
  BuiltinData ->
  -- | Script hash of stake validator.
  BuiltinData ->
  -- | Script context.
  BuiltinData ->
  BuiltinUnit
wallet _ cs (unsafeFromBuiltinData -> sh :: ScriptHash) sc =
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
