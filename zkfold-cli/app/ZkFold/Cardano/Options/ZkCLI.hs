
module ZkFold.Cardano.Options.ZkCLI where

{-
main :: IO ()
main = toplevelExceptionHandler $ do
  Crypto.cryptoInit

  envCli <- getEnvCli

  GHC.mkTextEncoding "UTF-8" >>= GHC.setLocaleEncoding
#ifdef UNIX
  _ <- setFileCreationMask (otherModes `unionFileModes` groupModes)
#endif
  co <- Opt.customExecParser pref (opts envCli)

  orDie (docToText . renderClientCommandError) $ runClientCommand co
-}

{-
opts :: EnvCli -> ParserInfo ClientCommand
opts envCli =
  Opt.info (parseClientCommand envCli <**> Opt.helper) $
    mconcat
      [ Opt.fullDesc
      , Opt.header $
          mconcat
            [ "cardano-cli - General purpose command-line utility to interact with cardano-node."
            , " Provides specific commands to manage keys, addresses, build & submit transactions,"
            , " certificates, etc."
            ]
      ]

pref :: ParserPrefs
pref =
  Opt.prefs $
    mconcat
      [ showHelpOnEmpty
      , helpEmbedBriefDesc PP.align
      , helpRenderHelp customRenderHelp
      ]
-}


{-
pCmds :: ShelleyBasedEra era -> EnvCli -> Parser (Cmds era)
pCmds era envCli = do
  asum $
    catMaybes
      [ Just (AddressCmds <$> pAddressCmds envCli)
      , Just (KeyCmds <$> pKeyCmds)
      , fmap GenesisCmds <$> pGenesisCmds era envCli
      , fmap GovernanceCmds <$> pGovernanceCmds era
      , Just (NodeCmds <$> pNodeCmds)
      , fmap QueryCmds <$> pQueryCmds era envCli
      , fmap StakeAddressCmds <$> pStakeAddressCmds era envCli
      , fmap StakePoolCmds <$> pStakePoolCmds era envCli
      , fmap TextViewCmds <$> pTextViewCmds
      , fmap TransactionCmds <$> pTransactionCmds era envCli
      ]
-}

{-
pTransactionBuildCmd
  :: ShelleyBasedEra era -> EnvCli -> Maybe (Parser (TransactionCmds era))
pTransactionBuildCmd sbe envCli = do
  era' <- forEraMaybeEon (toCardanoEra sbe)
  pure $
    subParser "build" $
      Opt.info (pCmd era') $
        Opt.progDescDoc $
          Just $
            mconcat
              [ pretty @String "Build a balanced transaction (automatically calculates fees)"
              , line
              , line
              , H.yellow $
                  mconcat
                    [ "Please note "
                    , H.underline "the order"
                    , " of some cmd options is crucial. If used incorrectly may produce "
                    , "undesired tx body. See nested [] notation above for details."
                    ]
              ]
 where
  pCmd era' = do
    fmap TransactionBuildCmd $
      TransactionBuildCmdArgs era'
        <$> ( LocalNodeConnectInfo
                <$> pConsensusModeParams
                <*> pNetworkId envCli
                <*> pSocketPath envCli
            )
        <*> optional pScriptValidity
        <*> optional pWitnessOverride
        <*> some (pTxIn sbe AutoBalance)
        <*> many pReadOnlyReferenceTxIn
        <*> many pRequiredSigner
        <*> many pTxInCollateral
        <*> optional pReturnCollateral
        <*> optional pTotalCollateral
        <*> many pTxOut
        <*> pChangeAddress
        <*> optional (pMintMultiAsset sbe AutoBalance)
        <*> optional pInvalidBefore
        <*> pInvalidHereafter sbe
        <*> many (pCertificateFile AutoBalance)
        <*> many (pWithdrawal AutoBalance)
        <*> pTxMetadataJsonSchema
        <*> many
          ( pScriptFor
              "auxiliary-script-file"
              Nothing
              "Filepath of auxiliary script(s)"
          )
        <*> many pMetadataFile
        <*> pFeatured (toCardanoEra sbe) (optional pUpdateProposalFile)
        <*> pVoteFiles sbe AutoBalance
        <*> pProposalFiles sbe AutoBalance
        <*> pTreasuryDonation sbe
        <*> pTxBuildOutputOptions

-}
