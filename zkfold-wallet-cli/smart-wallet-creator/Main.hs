{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import           Cardano.Api
import           Cardano.Api.Shelley                         (PlutusScript (..))
import           Codec.Serialise                             as CBOR
import           Control.Monad                               (void)
import           Data.Aeson
import qualified Data.ByteString.Base64.URL                  as B64
import qualified Data.ByteString.Char8                       as C8
import qualified Data.ByteString.Lazy                        as BL
import           Data.Proxy
import           Data.String                                 (IsString (..))
import           Flat.Types                                  ()
import           GHC.Generics                                (Generic, Par1 (..), U1 (..), type (:*:) (..))
import qualified GHC.Generics                                as G
import           Options.Applicative
import           PlutusLedgerApi.Common                      as V3
import           PlutusLedgerApi.V3                          as V3
import           PlutusTx                                    (CompiledCode, compile, liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude                            (BuiltinUnit)
import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import           System.Directory                            (createDirectoryIfMissing, getCurrentDirectory)
import           System.Environment                          (getArgs)
import           System.FilePath                             (takeFileName, (</>))

import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..), zero)
import qualified ZkFold.Base.Algebra.Basic.Number            as Number
import           ZkFold.Base.Algebra.Basic.Number            (Natural, type (^))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Base.Algebra.EllipticCurve.Class     (CyclicGroup (..))
import           ZkFold.Base.Data.Vector                     (Vector)
import           ZkFold.Base.Protocol.NonInteractiveProof as NP   (NonInteractiveProof (..))
import           ZkFold.Base.Protocol.Plonkup                (Plonkup (..))
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret  (PlonkupProverSecret (..))
import           ZkFold.Base.Protocol.Plonkup.Utils          (getParams, getSecrectParams)
import           ZkFold.Base.Protocol.Plonkup.Witness        (PlonkupWitnessInput (..))
import           ZkFold.Cardano.OffChain.Plonkup             (PlonkupN, mkInput, mkProof, mkSetup)
import           ZkFold.Cardano.OnChain.BLS12_381.F          (toF)
import           ZkFold.Cardano.OnChain.Plonkup              (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data         (InputBytes, ProofBytes (..), SetupBytes)
import           ZkFold.Cardano.UPLC.Wallet                  (SpendingCreds (..), WalletRedeemer (..), WalletSetup (..),
                                                              Web2Creds (..), untypedWallet)
import           ZkFold.Symbolic.Algorithms.RSA
import           ZkFold.Symbolic.Cardano.Contracts.SmartWallet
import           ZkFold.Symbolic.Class                       (Symbolic (..))
import qualified ZkFold.Symbolic.Compiler                    as C
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..))
import           ZkFold.Symbolic.Data.Bool                   (Bool (..), true)
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Eq                     (Eq (..))
import           ZkFold.Symbolic.Data.FieldElement           (FieldElement)
import           ZkFold.Symbolic.Data.Input
import           ZkFold.Symbolic.Data.JWT                    as JWT
import           ZkFold.Symbolic.Data.JWT.Google
import           ZkFold.Symbolic.Data.JWT.RS256              as RS256
import           ZkFold.Symbolic.Data.UInt                   (UInt)
import           ZkFold.Symbolic.Data.VarByteString
import           ZkFold.Symbolic.Interpreter


main :: IO ()
main = do
    print bytes
    where
        bytes = mkSetup $ expModSetup @BuiltinByteString zero


