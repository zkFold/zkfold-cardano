{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.WalletImproved (
  module ZkFold.Cardano.UPLC.Wallet.Types,
  web2Auth,
  checkSig,
  wallet,
) where

import           Data.Function                       ((&))
import           PlutusLedgerApi.Data.V2             (toSOPList)
import           PlutusLedgerApi.V1.Value            (currencySymbol, valueOf)
import           PlutusLedgerApi.V3
import           PlutusLedgerApi.V3.Contexts
import qualified PlutusTx.AssocMap                   as AssocMap
import qualified PlutusTx.Builtins.Internal          as BI
import qualified PlutusTx.Builtins          as B
import qualified PlutusTx.Builtins.HasOpaque as BH
import           PlutusTx.Prelude                    hiding (show, toList, (*), (+))
import           PlutusTx.Show
import           PlutusTx.Trace

import           ZkFold.Algebra.Class                (MultiplicativeSemigroup (..))
import           ZkFold.Cardano.OnChain.BLS12_381.F  (toInput)
import           ZkFold.Cardano.OnChain.Plonkup      (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data (SetupBytes)
import           ZkFold.Cardano.UPLC.Wallet.Internal (base64urlEncode)
import           ZkFold.Cardano.UPLC.Wallet.Types
import           ZkFold.Protocol.NonInteractiveProof (NonInteractiveProof (..))

{--
Vladimir Sinyakov, [26.08.2025 20:43]
У minting скрипта всё то же самое, кроме:
1) Мы должны найти reference input, в котором лежим beacon токен.
2) В датуме этого инпута должен лежать SetupBytes, которые мы используем.
3) Саму цепь нужно параметризовать гугл ключом, а не передавать его в качестве входа цепи (как сейчас).

Vladimir Sinyakov, [26.08.2025 20:45]
Beacon токен должен иметь one-shot minting policy, то есть в скрипте мы должны проверять, что тратится какой-то конкретный инпут (у нас уже такой скрипт реализован, см. zkfold-cardano).
--}

-- TODO: Account for rotation of public keys
-- TODO: Check the client Id
-- TODO: Check the suffix length (must be a predefined size)
-- TODO: Do we need to split bytestrings further due to ledger rules?
{-# INLINEABLE web2Auth #-}

-- | Mints tokens paramterized by the user's email and a public key selected by the user.
web2Auth ::
  -- | Beacon token Currency Symbol (or minting policy id) 
  BuiltinData ->
  -- | Beacon token name 
  BuiltinData ->
  -- | 'Web2Creds'.
  BuiltinData ->
  -- | 'ScriptContext'.
  BuiltinData ->
  BuiltinUnit
web2Auth beaconSymbol beaconName (unsafeFromBuiltinData -> Web2Creds {..}) sc =
  check
    $ let
        encodedJwt = base64urlEncode jwtHeader <> "." <> base64urlEncode (jwtPrefix <> w2cEmail <> jwtSuffix)
        jwtHash = sha2_256 encodedJwt
        publicInput = toInput jwtHash * toInput bs
       in
        -- Check that the user knows an RSA signature for a JWT containing the email
         verify @PlonkupPlutus expModCircuit [publicInput] proof
          -- Check that we mint a token with the correct name
          && AssocMap.lookup (toBuiltinData symb) txInfoMint
          == Just (toBuiltinData $ AssocMap.singleton tn (1 :: Integer))
          && elem (PubKeyHash bs) txInfoSignatories
 where
  ctx = unsafeFromBuiltinData sc :: ScriptContext

  -- tx reference inputs
  refInputs = map txInInfoResolved . txInfoReferenceInputs . scriptContextTxInfo $ ctx

  symbols = (fmap unCurrencySymbol . AssocMap.keys . getValue . txOutValue) <$> refInputs
  tokens = (fmap (fmap unTokenName . AssocMap.keys) . AssocMap.elems . getValue . txOutValue) <$> refInputs

  correctCurrencySymbol = CurrencySymbol $ unsafeFromBuiltinData beaconSymbol
  correctTokenName = TokenName $ unsafeFromBuiltinData beaconName 

  beaconInput = find (\ri -> valueOf (txOutValue ri) correctCurrencySymbol correctTokenName > 0) $ trace (show $ length refInputs) refInputs

  -- find beacon datum
  beaconDatum = trace ("Beacon input: " <> show (symbols, unCurrencySymbol correctCurrencySymbol, tokens, unTokenName correctTokenName)) $ fmap txOutDatum beaconInput

  -- decode beacon datum
  setupBytesMap =
      case beaconDatum of
        Just (OutputDatum datum) -> case fromBuiltinData $ getDatum datum of
                                      Nothing -> traceError "Decoding datum failed"
                                      Just m  -> m
        Nothing -> traceError "Missing beacon token."
        _ -> traceError "Incorrect datum."

  setupBytes =
      case AssocMap.lookup (toBuiltinData kid) setupBytesMap of
        Just res -> res
        Nothing -> traceError $ "No key id " <> show kid <> " found. Known ids " <> show (AssocMap.keys setupBytesMap)

  expModCircuit :: SetupBytes
  expModCircuit = unsafeFromBuiltinData setupBytes

  txInfoL = BI.unsafeDataAsConstr sc & BI.snd
  txInfo = txInfoL & BI.head & BI.unsafeDataAsConstr & BI.snd
  redL = txInfoL & BI.tail
  Web2Auth JWTParts {..} proof tn@(TokenName bs) (KeyId kid) = redL & BI.head & unsafeFromBuiltinData
  (MintingScript symb) = redL & BI.tail & BI.head & unsafeFromBuiltinData
  txInfoMintL =
    txInfo
      & BI.tail
      & BI.tail
      & BI.tail
      & BI.tail
  txInfoMint :: Map BuiltinData BuiltinData =
    txInfoMintL
      & BI.head
      & unsafeFromBuiltinData
  txInfoSignatories :: [PubKeyHash] =
    txInfoMintL
      & BI.tail
      & BI.tail
      & BI.tail
      & BI.tail
      & BI.head
      & unsafeFromBuiltinData

{-# INLINEABLE checkSig #-}
checkSig ::
  -- | 'CurrencySymbol'.
  BuiltinData ->
  -- | 'ScriptContext'.
  BuiltinData ->
  BuiltinUnit
checkSig (unsafeFromBuiltinData -> (symb :: CurrencySymbol)) sc =
  check
    $
    -- extract the value of the i-th output
    let v = txOutValue $ txInfoOutputs !! i
     in -- j-th pubKeyHash is equal to the tokenName of the currency symbol
        valueOf v symb (TokenName $ getPubKeyHash $ txInfoSignatories !! j) > 0
 where
  txInfoL = BI.unsafeDataAsConstr sc & BI.snd
  txInfo = txInfoL & BI.head & BI.unsafeDataAsConstr & BI.snd
  Signature i j = txInfoL & BI.tail & BI.head & unsafeFromBuiltinData
  txInfoOutputsL =
    txInfo
      & BI.tail
      & BI.tail
  txInfoOutputs = txInfoOutputsL & BI.head & unsafeFromBuiltinData
  txInfoSignatories :: [PubKeyHash] =
    txInfoOutputsL
      & BI.tail
      & BI.tail
      & BI.tail
      & BI.tail
      & BI.tail
      & BI.tail
      & BI.head
      & unsafeFromBuiltinData

{-# INLINEABLE wallet #-}
wallet ::
  -- | Currency symbol of user's minting script.
  BuiltinData ->
  -- | Script hash of stake validator.
  BuiltinData ->
  -- | Script context.
  BuiltinData ->
  BuiltinUnit
wallet cs (unsafeFromBuiltinData -> sh :: ScriptHash) sc =
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
