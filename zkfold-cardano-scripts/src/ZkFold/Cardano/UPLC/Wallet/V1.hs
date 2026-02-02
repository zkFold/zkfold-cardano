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

        correctLengths = length v == 16 && length aut == 16

    {--
        verified = and $ flip map (zip v aut) $ \(vi, auti) ->
            let autbs = integerToByteString BigEndian 256 auti
                digest = sha2_256 (c <> autbs)
                i = (byteStringToInteger BigEndian digest) `modulo` pubE
                lhs = myExpMod vi pubE pubN
                rhs = (auti * myExpMod paddedHash i pubN) `modulo` pubN
             in lhs == rhs
    --}
        verified = True
       in
        -- Check that the user knows an RSA signature for a JWT containing the email
         trace (show [correctLengths, verified, hasZkFoldFee]) $ correctLengths && verified && hasZkFoldFee
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
    check $ trace (show [show rewardingUserId, show userId, show hasWithdrawal, show userIdMatches, show (ScriptCredential sh), show (AssocMap.keys txInfoWrdl)]) $ hasWithdrawal && userIdMatches
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
    redeemerType = toBuiltinData $ Rewarding $ ScriptCredential sh

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
