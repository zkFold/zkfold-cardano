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

        correctLengths = length v == 2 && length aut == 2

        -- verified = and $ flip map (zip v aut) $ \(vi, auti) ->
        verified = flip map (zip v aut) $ \(vi, auti) ->
            let i = (byteStringToInteger BigEndian $ sha2_256 (encodeUtf8 $ show c <> show aut)) `modulo` pubE
                -- lhs = myExpMod vi pubE pubN
                -- rhs = (auti * myExpMod paddedHash i pubN) `modulo` pubN
             -- in lhs == rhs
             in i
       in
        -- Check that the user knows an RSA signature for a JWT containing the email
         correctLengths && traceError (show verified) && hasZkFoldFee
 where
  -- tx reference inputs
  refInput = txInfo & BI.tail & BI.head & BI.unsafeDataAsList & BI.head -- TxInInfo
  refInputResolved = refInput & BI.unsafeDataAsConstr & BI.snd & BI.tail & BI.head -- TxOut
  txOutL = refInputResolved & BI.unsafeDataAsConstr & BI.snd & BI.tail

    {--
  -- find beacon datum
  beaconDatum = txOutL & BI.tail & BI.head & unsafeFromBuiltinData

  -- decode beacon datum
  pubkeyMap =
      case beaconDatum of
        OutputDatum datum -> unsafeFromBuiltinData $ getDatum datum
        _                 -> error ()
--}

  pubkeyMap = AssocMap.safeFromList
                [ (toBuiltinData ("7bf595489a0bb158b085e23e7b52bf9891e04538" :: BuiltinByteString), PubKey 65537 0xC4E4A4DD6B8EE5487D08C672C6A124894630294999C991B60BA384117E43639A0F2EC004458B3F5CAFD1EFBC0E48C4A7DEDE55E8386AC7DC140ACD2AFF2C619EB2B5983B84BCC524E407953D4A9960CF41810F21AD6389DC6AE4C2F89646F84A40733620DCEE1930EB9AE101D4645F7007AFC4AE59587DDEE8AA5627CC8E74893D00B239ECC77542582CE002DE293DC5EC7F84008517C86B6B7255FA3DA8DD2346AF1CD30B7ED6F27A1380483281B67D65E955D70D0F94FB5C731ED59B323F89234A79253BEEA90C9F7E87833852B797679B23893D6DC168BB83B6005F9027880C0B12EF9F6AE364D8880C104BE2DB420A60E20C2C5FBC516AFCB7AEA95A2EE1)
                , (toBuiltinData ("9544d0ff0590f025301643f3275bf68776765822" :: BuiltinByteString), PubKey 65537 0x9FFEBD1039D821B46D6051A96FB65AEB79E065CC77153DEB848506267A4BDD3A5F92A23F7EDBC25742D92091F1DB5FA0EF48FD0E76F128EEA4C0218A15FC1FC6DB3A2F518DBF3288D55A8CF7727C22DEFDFCCD2C489BE8BF0F52C0233B8466B16AD7483AC63BDAE21670632EA062CD9CFD0C12A000C8BED73E9BF5561A1626109B76ECF38D684BC201ED08052AEC610D6F25A90E69B63E18C2F8307CD0A7C89C8A5ABF8B516FE5BF9434E3C3E558C7B0FC96FD351422F3FEE488F6A2B6BBFBB8236AB122650D17C510C2BAFDCB50504BD3C7538708E9911789C4F53692A9978293D5FE467EF8A0E4D17805480F4C1AC562D08187898D5E7C3235A467D50A35F7)
                ]

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


{-# INLINEABLE myExpMod #-}
-- TODO: replace with builtin expMod when it's available
myExpMod :: Integer -> Integer -> Integer -> Integer
myExpMod base power mod
  | power == 0 = 1
  | even power = (halfPow * halfPow) `modulo` mod
  | otherwise = (halfPow * halfPow * base) `modulo` mod
 where
  halfPow = myExpMod base (power `divide` 2) mod

{-# INLINEABLE wallet #-}
wallet ::
  -- | Dummy parameter for extra addresses
  BuiltinData ->
  -- | User's email
  BuiltinData ->
  -- | Script context.
  BuiltinData ->
  BuiltinUnit
wallet _ email sc = check True
