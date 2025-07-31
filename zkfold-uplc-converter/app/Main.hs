{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NoStarIsType       #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE ViewPatterns       #-}

module Main where

import qualified Cardano.Api                              as Cardano
import qualified Cardano.Api.Shelley                      as Cardano
import           Control.Applicative                      (Applicative, asum, pure, some, (<**>), (<*>))
import           Control.Exception                        (displayException)
import           Control.Monad                            ((>=>), (>>), (>>=))
import qualified Data.Aeson                               as Aeson
import           Data.Bifunctor                           (first)
import           Data.Bool                                (otherwise)
import           Data.ByteString                          (ByteString)
import qualified Data.ByteString.Lazy                     as BS
import           Data.Constraint                          (withDict)
import           Data.Constraint.Nat                      (plusNat, timesNat)
import           Data.Eq                                  (Eq, (==))
import           Data.Foldable                            (Foldable, fold, toList)
import           Data.Function                            (flip, id, ($), (.))
import           Data.Functor                             ((<$>))
import           Data.Functor.Rep                         (Rep, Representable, tabulate)
import           Data.List.NonEmpty                       (NonEmpty (..), fromList)
import           Data.Map.Monoidal                        (keys)
import           Data.Maybe                               (Maybe (..))
import           Data.Monoid                              (Last (..), Monoid, mempty)
import           Data.Ord                                 (max)
import           Data.Semigroup                           ((<>))
import           Data.Traversable                         (traverse)
import           GHC.TypeNats                             (KnownNat, Natural, SNat, fromSNat, type (*), type (+),
                                                           withKnownNat, withSomeSNat)
import qualified Options.Applicative                      as O
import qualified PlutusLedgerApi.V3                       as Plutus
import qualified PlutusTx                                 as Plutus
import qualified PlutusTx.Prelude                         as Plutus
import           Prelude                                  (error)
import           System.IO                                (IO)
import qualified System.IO                                as IO
import qualified System.IO.Temp                           as Temp
import qualified System.IO.Unsafe                         as Unsafe
import           System.OsPath                            (OsPath, (<.>))
import qualified System.OsPath                            as OS
import qualified System.Process                           as P

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.BLS12_381   (BLS12_381_G1_Point, BLS12_381_G2_Point, Fr)
import           ZkFold.Algebra.EllipticCurve.Class       (CyclicGroup, ScalarFieldOf)
import           ZkFold.Algebra.Polynomial.Univariate     (PolyVec)
import           ZkFold.ArithmeticCircuit                 (ArithmeticCircuit, acContext, acSizeL, acSizeN)
import           ZkFold.ArithmeticCircuit.Context         (acLookup)
import           ZkFold.ArithmeticCircuit.Lookup          (LookupTable (..), LookupType (LookupType))
import           ZkFold.Cardano.OffChain.Plonkup          (mkSetup)
import           ZkFold.Cardano.UPLC.PlonkupVerifierToken (plonkupVerifierTokenCompiled)
import           ZkFold.Data.ByteString                   (Binary)
import           ZkFold.Prelude                           (length)
import           ZkFold.Protocol.NonInteractiveProof      (setupProve, setupVerify)
import           ZkFold.Protocol.Plonkup                  (Plonkup (..))
import           ZkFold.Protocol.Plonkup.Utils            (getParams, getSecretParams)
import           ZkFold.Symbolic.Class                    (Arithmetic)
import           ZkFold.Symbolic.UPLC.Converter           (ScriptType (..), SomeCircuit (..), convert)
import           ZkFold.UPLC.Term                         (VersionedProgram (..))

data InputType = UPLC | TPLC | PIR | UPLC'Flat | UPLC'CBOR deriving Eq

data Input = FileInput (NonEmpty OsPath) | StdIn InputType

data OutputType = Circuit | PlutusVerifier | ProverInfo deriving Eq

data Output = FileOutput (OutputType -> Last OsPath) | StdOut OutputType

data Action = Act
  { actInput      :: Input
  , actScriptType :: ScriptType
  , actOutput     :: Output
  }

main :: IO ()
main = do
  Act {..} <- O.execParser $
    O.info (actionParser <**> O.helper) (O.fullDesc <> O.progDesc "UPLC converter!")
  Program _ term <- withBinaryInput UPLC actInput \_bs -> error "TODO: UPLC parser"
  case convert term actScriptType of
    SomeCircuit !circuit -> do
      withBinaryOutput Circuit actOutput (Aeson.encode circuit)
      withPlonkup circuit \plonkup -> do
        withBinaryOutput PlutusVerifier actOutput $ compiledToJSON (plutusVerifier plonkup)
        withBinaryOutput ProverInfo actOutput $ Aeson.encode (setupProve plonkup)

plutusVerifier ::
  (Foldable o, Representable o, Representable i, Binary (Rep i), KnownPlonkup n) =>
  Plonkup i o n BLS12_381_G1_Point BLS12_381_G2_Point ByteString (PolyVec Fr) ->
  Plutus.CompiledCode (Plutus.BuiltinData -> Plutus.BuiltinUnit)
plutusVerifier = plonkupVerifierTokenCompiled . mkSetup . setupVerify

-- Helpers to compute proper @n@ for Plonkup.
-- Maybe worth moving somewhere into symbolic-base

type KnownPlonkup n = (KnownNat n, KnownNat (4 * n + 6))

withPlonkup ::
  forall p q a i o r.
  (Arithmetic a, Foldable o, Representable o, CyclicGroup p, CyclicGroup q) =>
  (ScalarFieldOf p ~ a, ScalarFieldOf q ~ a) =>
  ArithmeticCircuit a i o ->
  (forall n. KnownPlonkup n => Plonkup i o n p q ByteString (PolyVec a) -> r) ->
  r
withPlonkup circuit k =
  withSomeSNat plonkupCircuitSize \(snat :: SNat n) ->
    withKnownNat snat $
      let (omega, k1, k2) = getParams (fromSNat snat)
          (gs, h1) = withDict (plusNat @n @6) (getSecretParams @n zero)
       in withDict (timesNat @4 @n) $
        withDict (plusNat @(4 * n) @6) $
          k (Plonkup omega k1 k2 circuit h1 gs)
 where
  plonkupCircuitSize =
    (acSizeN circuit + acSizeL circuit + acSizeO circuit) `max`
    sum [ lookupSize lt | LookupType lt <- keys $ acLookup (acContext circuit) ]

  lookupSize :: Arithmetic a => LookupTable a f -> Natural
  lookupSize (Ranges s)      = sum [ toConstant (hi - lo) + 1 | (lo, hi) <- toList s ]
  lookupSize (Product lt mt) = lookupSize lt * lookupSize mt
  lookupSize (Plot _ lt)     = lookupSize lt

acSizeO :: forall a i o. (Foldable o, Representable o) => ArithmeticCircuit a i o -> Natural
acSizeO _ = length (tabulate @o id)

-- I/O helpers

withBinaryOutput :: OutputType -> Output -> BS.ByteString -> IO ()
withBinaryOutput planned = \case
  StdOut actual | actual == planned -> BS.putStr
  FileOutput (($ planned) -> Last (Just path)) ->
    (OS.decodeUtf path >>=) . flip BS.writeFile
  _ -> \_ -> pure ()

-- Maybe worth using Plutus API directly since we already depend on it.

withBinaryInput :: InputType -> Input -> (BS.ByteString -> IO r) -> IO r
withBinaryInput expected = \case
  StdIn actual
    | actual == expected -> (BS.getContents >>=)
    | actual == UPLC -> withNeededFile \neededPath ->
        IO.getContents >>= P.readProcess "plutus" ["--stdin", "-o", neededPath]
    | otherwise -> withNeededFile \neededPath ->
        withTempFile ("input" <.?> inputExt actual) \inputPath inputHandle -> do
          BS.getContents >>= BS.hPut inputHandle
          P.callProcess "plutus" [inputPath, "-o", neededPath]
  FileInput (single :| []) | OS.takeExtension single == inputExt expected ->
    (OS.decodeUtf single >>=) . (BS.readFile >=>)
  FileInput paths -> withNeededFile \neededPath -> do
    inputPaths <- traverse OS.decodeUtf paths
    P.callProcess "plutus" $ toList inputPaths <> ["-o", neededPath]
 where
  inputExt :: InputType -> OsPath
  inputExt =
    Unsafe.unsafePerformIO . OS.encodeUtf . \case
      UPLC -> "uplc"
      TPLC -> "tplc"
      PIR -> "pir"
      UPLC'Flat -> "uplc-flat"
      UPLC'CBOR -> "uplc-cbor"

  (<.?>) :: IO.FilePath -> OS.OsString -> IO IO.FilePath
  root <.?> ext = do
    pathRoot <- OS.encodeUtf root
    OS.decodeUtf (pathRoot <.> ext)

  withTempFile :: IO IO.FilePath -> (IO.FilePath -> IO.Handle -> IO r) -> IO r
  withTempFile template continue = do
    templateResult <- template
    Temp.withSystemTempFile templateResult continue

  withNeededFile :: (IO.FilePath -> IO a) -> (BS.ByteString -> IO r) -> IO r
  withNeededFile onPath onData =
    withTempFile ("preprocessed" <.?> inputExt expected) \fp h ->
      onPath fp >> BS.hGetContents h >>= onData

-- Smart contract serialization. Maybe worth moving to zkfold-scripts-common

compiledToJSON ::
  Plutus.CompiledCode (Plutus.BuiltinData -> Plutus.BuiltinUnit) -> BS.ByteString
compiledToJSON =
  Cardano.textEnvelopeToJSON Nothing
  . Cardano.PlutusScriptSerialised @Cardano.PlutusScriptV3
  . Plutus.serialiseCompiledCode

-- CLI args parser

actionParser :: O.Parser Action
actionParser =
  Act
    <$> asum
      [ FileInput . fromList <$> some scriptPathParser
      , StdIn <$> inputTypeParser
      ]
    <*> scriptTypeParser
    <*> asum
      [ FileOutput . fold <$> some fileOutputParser
      , StdOut <$> outputTypeParser
      ]
  where
    scriptPathParser = O.argument pathRead $
      O.metavar "SCRIPT_FILE"
      <> O.help "A path to the source file(s). Input type is deduced from extension."

    inputTypeParser = asum
      [ O.flag' TPLC (O.long "tplc" <> O.help "stdin is Typed Plutus Core with Names")
      , O.flag' PIR (O.long "pir" <> O.help "stdin is PIR with Names")
      , O.flag' UPLC'Flat
        ( O.long "uplc-flat"
          <> O.help "stdin is Untyped Plutus Core with NamedDeBruijn serialised in Flat"
        )
      , O.flag' UPLC'CBOR
        ( O.long "uplc-cbor"
          <> O.help "stdin is Untyped Plutus Core with DeBruijn serialised in CBOR"
        )
      , O.flag UPLC UPLC (O.long "uplc" <> O.help "stdin is Untyped Plutus Core with Names (default)")
      ]

    scriptTypeParser = asum
      [ O.flag' SpendingV1 (O.long "spending-v1" <> O.help "Input is a Spending V1 script")
      , O.flag' OtherV1 (O.long "other-v1" <> O.help "Input is a Minting, Certifying or Rewarding V1 script")
      , O.flag V3 V3 (O.long "v3" <> O.help "Input is a V3 script (default)")
      ]

    fileOutputParser = asum
      [ at Circuit <$> pathOption
        ( O.metavar "CIRCUIT_FILE"
          <> O.long "circuit"
          <> O.short 'c'
          <> O.help "File to write circuit to"
        )
      , at PlutusVerifier <$> pathOption
        ( O.metavar "VERIFIER_FILE"
          <> O.long "verifier"
          <> O.short 'v'
          <> O.help "File to write verifier to"
        )
      , at ProverInfo <$> pathOption
        ( O.metavar "PROVER_INFO_FILE"
          <> O.long "prover-info"
          <> O.short 'p'
          <> O.help "File to write prover info to"
        )
      ]

    outputTypeParser = asum
      [ O.flag' PlutusVerifier
          (O.long "print-verifier" <> O.help "Write verifier to stdout")
      , O.flag' ProverInfo
          (O.long "print-prover-info" <> O.help "Write prover info to stdout")
      , O.flag Circuit Circuit
          (O.long "print-circuit" <> O.help "Write circuit to stdout (default)")
      ]

    at :: (Eq a, Applicative f, Monoid (f b)) => a -> b -> a -> f b
    at x y z
      | x == z = pure y
      | otherwise = mempty

    pathRead :: O.ReadM OsPath
    pathRead = O.eitherReader (first displayException . OS.encodeUtf)

    pathOption :: O.Mod O.OptionFields OsPath -> O.Parser OsPath
    pathOption = O.option pathRead
