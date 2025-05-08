module ZkFold.Cardano.PlonkupVerifierToken.Transaction.Minting (tokenMinting, Transaction(..)) where

import           Cardano.Api                              (AssetName (..))
import           Data.Aeson                               (decode)
import qualified Data.ByteString.Lazy                     as BL
import           Data.Coerce                              (coerce)
import           Data.Maybe                               (fromJust)
import           GeniusYield.GYConfig                     (GYCoreConfig (..), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V3                       (fromBuiltin)
import           Prelude
import           System.FilePath                          ((</>))
import qualified System.IO                                as IO

import           ZkFold.Cardano.Atlas.Utils               (SubmittedTx (..), wrapUpSubmittedTx)
import           ZkFold.Cardano.Examples.EqualityCheck    (EqualityCheckContract (..), equalityCheckVerificationBytes)
import qualified ZkFold.Cardano.OnChain.BLS12_381.F       as F
import           ZkFold.Cardano.Options.Common            (CoreConfigAlt, SigningKeyAlt, TxIdAlt, fromCoreConfigAltIO,
                                                           fromSigningKeyAltIO, fromTxIdAltIO)
import           ZkFold.Cardano.UPLC.PlonkupVerifierToken (plonkupVerifierTokenCompiled)

data Transaction = Transaction
    { curPath        :: !FilePath
    , coreCfgAlt     :: !CoreConfigAlt
    , requiredSigner :: !SigningKeyAlt
    , changeAddress  :: !GYAddress
    , outAddress     :: !GYAddress
    , txidSetup      :: !TxIdAlt
    , outFile        :: !FilePath
    }

-- | Sending a tokens script to the address.
sendMintTokens :: GYNetworkId         ->
                  GYProviders         ->
                  FilePath            ->
                  -- ^ Path to 'assets' directory.
                  GYPaymentSigningKey ->
                  -- ^ Signing key for wallet funding this Tx.
                  GYAddress           ->
                  -- ^ Change address for wallet funding this Tx.
                  GYAddress           ->
                  -- ^ Beneficiary receiving token.
                  GYScript PlutusV3   ->
                  -- ^ Parameterized PlonkupVerifierToken script.
                  GYTxId              ->
                  -- ^ Setup reference TxId.
                  GYRedeemer          ->
                  -- ^ Redeemer containing proof.
                  GYAssetClass        ->
                  -- ^ Token to mint.
                  FilePath            ->
                  -- ^ Path to output file.
                  IO ()
sendMintTokens nid providers assetsPath skey changeAddr sendTo validator txidSetup redeemer token outFile = do
  let w1 = User' skey Nothing changeAddr

  let txOutRefSetup = txOutRefFromTuple (txidSetup, 0)
      refScript     = GYMintReference @PlutusV3 txOutRefSetup validator

  let tokenValue = valueSingleton token 1
      tokenName | GYToken _ t <- token = t
                | otherwise            = error "absurd"

  pkh <- addressToPubKeyHashIO changeAddr

  let skeleton = mustHaveOutput (GYTxOut sendTo tokenValue Nothing Nothing)
              <> mustMint refScript redeemer tokenName 1
              <> mustBeSignedBy pkh

  tx <- runGYTxGameMonadIO nid
                           providers $
                           asUser w1 $ do
                             txbody <- buildTxBody skeleton
                             txid   <- signAndSubmitConfirmed txbody
                             return $ SubmittedTx txid (Just $ txBodyFee txbody)

  let (pidStg, tnStg) = asTuple token
  putStrLn $ "Policy ID: " ++ pidStg
  putStrLn $ "Token Name: " ++ tnStg
  IO.writeFile (assetsPath </> "lastMintedToken.txt") $ "\"" ++ pidStg ++ "." ++ tnStg ++ "\""

  wrapUpSubmittedTx (assetsPath </> outFile) tx

tokenMinting :: Transaction -> IO ()
tokenMinting (Transaction path coreCfg' sig changeAddr sendTo txidSetup' outFile) = do
  let assetsPath = path </> "assets"
      testData   = path </> "test-data"

  coreCfg   <- fromCoreConfigAltIO coreCfg'
  skey      <- fromSigningKeyAltIO sig
  txidSetup <- fromTxIdAltIO assetsPath txidSetup'

  let nid = cfgNetworkId coreCfg

  EqualityCheckContract{..} <- fromJust . decode <$> BL.readFile (testData </> "plonkup-raw-contract-data.json")

  let (setup, input, proof) = equalityCheckVerificationBytes x ps targetValue
      plonkupTokenValidator = scriptFromPlutus @PlutusV3 $ plonkupVerifierTokenCompiled setup
      policyId              = mintingPolicyId plonkupTokenValidator
      assetName             = AssetName $ fromBuiltin $ F.fromInput $ head input
      token                 = GYToken policyId (coerce assetName)
      redeemer              = redeemerFromPlutusData proof

  withCfgProviders coreCfg "zkfold-cli" $ \providers -> sendMintTokens
                                                          nid
                                                          providers
                                                          assetsPath
                                                          skey
                                                          changeAddr
                                                          sendTo
                                                          plonkupTokenValidator
                                                          txidSetup
                                                          redeemer
                                                          token
                                                          outFile


------- :Helpers: -------

-- | GYAssetClass as a tuple of strings.
asTuple :: GYAssetClass -> (String, String)
asTuple GYLovelace    = ("", "Lovelace")
asTuple (GYToken p t) = (trim $ show p, trim . show $ tokenNameToHex t)
  where
    trim = reverse . drop 1 . reverse . drop 1
