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
import qualified PlutusTx.Prelude                    as Plutus
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
pad = 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff003031300d0609608648016503040201050004200000000000000000000000000000000000000000000000000000000000000000

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

        c = integerToByteString BigEndian 256 paddedHash

        correctLengths = length v == 2 && length aut == 2

        verified = and $ flip map (zip (Plutus.tail v) (Plutus.tail aut)) $ \(vi, auti) ->
            let autbs = integerToByteString BigEndian 256 auti
                digest = sha2_256 (c <> autbs)
                i = (byteStringToInteger BigEndian digest) `modulo` pubE
                lhs = myExpMod vi pubE pubN
                rhs = (auti * myExpMod paddedHash i pubN) `modulo` pubN
             in lhs == rhs
       in
        -- Check that the user knows an RSA signature for a JWT containing the email
         trace (show [correctLengths, verified, hasZkFoldFee]) $ correctLengths && verified && hasZkFoldFee
 where
    {--
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
--}

  pubkeyMap = AssocMap.safeFromList
                [ (toBuiltinData ("7bf595489a0bb158b085e23e7b52bf9891e04538" :: BuiltinByteString), PubKey 65537 0xC4E4A4DD6B8EE5487D08C672C6A124894630294999C991B60BA384117E43639A0F2EC004458B3F5CAFD1EFBC0E48C4A7DEDE55E8386AC7DC140ACD2AFF2C619EB2B5983B84BCC524E407953D4A9960CF41810F21AD6389DC6AE4C2F89646F84A40733620DCEE1930EB9AE101D4645F7007AFC4AE59587DDEE8AA5627CC8E74893D00B239ECC77542582CE002DE293DC5EC7F84008517C86B6B7255FA3DA8DD2346AF1CD30B7ED6F27A1380483281B67D65E955D70D0F94FB5C731ED59B323F89234A79253BEEA90C9F7E87833852B797679B23893D6DC168BB83B6005F9027880C0B12EF9F6AE364D8880C104BE2DB420A60E20C2C5FBC516AFCB7AEA95A2EE1)
                , (toBuiltinData ("8630a71bd6ec1c61257a27ff2efd91872ecab1f6" :: BuiltinByteString), PubKey 65537 0xaac9dc0ae5dbbe1acec26608bead70ecf2061cea56b908597b84de2281e59c98a7884daec870bbf88b405b97f73e0c34596e87defe0726d44a1f21eb9044048b739f3f45cb427dabb7acf863a576a753fe30e6aaaecfb0bf407a0223e8dc7bf6f650c453aac535225fd2a640b2d845667509bb3bdb63cacb84e5bc0b0ac82ab48779d0108ddfc647f99c71140a78911b2925cee26f9cf4fa1a3ce9e4c8eefd94ad80ba7cb9c28b5befe1dbdfba0f1713cf94a34ea0765a9ddd10403ccacd77c19ba5c2215e696c7dd8e9d1fa3de2f72ffeda473d6a4dd74f91720d82bbcf423638f1cf9a8dc94f25e4d5ff32efe8d416569af74b1e55dffe98af2d74f51b219f)
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
  | power == 1 = base `modulo` mod
  | even power = halfPow2
  | otherwise = (halfPow2 * base) `modulo` mod
 where
  halfPow = myExpMod base (power `divide` 2) mod
  halfPow2 = (halfPow * halfPow) `modulo` mod

{-# INLINEABLE wallet #-}
wallet ::
  -- | Dummy parameter for extra addresses
  BuiltinData ->
  -- | User ID
  BuiltinData ->
  -- | Script hash of the stake validator
  BuiltinData ->
  -- | Script context.
  BuiltinData ->
  BuiltinUnit
wallet _ userId (unsafeFromBuiltinData -> sh :: ScriptHash) sc =
    -- Check that there is a withdrawal and that the correct user ID was provided to the rewarding script in the redeemer
    check $ trace (show [show rewardingUserId, show userId, show hasWithdrawal, show userIdMatches]) $ hasWithdrawal && userIdMatches
  where
    txInfoL = BI.unsafeDataAsConstr sc & BI.snd
    txInfo = txInfoL & BI.head & BI.unsafeDataAsConstr & BI.snd

    txInfoWrdlL =
        txInfo
          & BI.tail
          & BI.tail
          & BI.tail
          & BI.tail
          & BI.tail
          & BI.tail

    txInfoWrdl :: Map BuiltinData BuiltinData
    txInfoWrdl = txInfoWrdlL
                   & BI.head
                   & unsafeFromBuiltinData

    hasWithdrawal = AssocMap.member (toBuiltinData $ ScriptCredential sh) txInfoWrdl

    redeemerMap :: Map BuiltinData BuiltinData
    redeemerMap =
        txInfoWrdlL
          & BI.tail
          & BI.tail
          & BI.tail
          & BI.head
          & unsafeFromBuiltinData

    redeemerType :: BuiltinData
    --redeemerType = toBuiltinData $ Rewarding $ ScriptCredential sh
    redeemerType = toBuiltinData $ Certifying 0 $ TxCertRegStaking (ScriptCredential sh) (Just 400000)

    showScriptPurpose :: ScriptPurpose -> BuiltinString
    showScriptPurpose (Spending tx) = "Spending "
    showScriptPurpose (Rewarding scr) = "Rewarding " <> show scr
    showScriptPurpose (Minting scr) = "Minting "
    showScriptPurpose (Certifying i (TxCertRegStaking cred lovelace)) = "Certifying " <> show i <> " " <> show lovelace
    showScriptPurpose (Certifying i _) = "Certifying OTHER " <> show i
    showScriptPurpose (Voting scr) = "Voting "
    showScriptPurpose (Proposing i scr) = "Proposing " <> show i 

    rewardingRedeemer :: BuiltinData
    rewardingRedeemer = case AssocMap.lookup redeemerType redeemerMap of
                          Nothing -> traceError (show (map (showScriptPurpose . unsafeFromBuiltinData) $ AssocMap.keys redeemerMap) <> " :: " <> show redeemerType)
                          Just r  -> r

    rewardingUserId =
        BI.unsafeDataAsConstr rewardingRedeemer
          & BI.snd
          & BI.tail
          & BI.head


    userIdMatches = rewardingUserId == userId
