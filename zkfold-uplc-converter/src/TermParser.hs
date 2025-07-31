{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}

module TermParser where

import qualified Text.Megaparsec as P
import Data.ByteString.Lazy (ByteString, pack)
import ZkFold.UPLC.Term (VersionedProgram (..), Term (..))
import Control.Applicative ((<*>), (<*), (<|>), (*>), asum)
import Data.Functor ((<$>), (<$), fmap)
import qualified Text.Megaparsec.Byte.Lexer as L
import qualified Text.Megaparsec.Byte as B
import Data.Function (($), (.))
import Data.List (zip, foldl1')
import ZkFold.UPLC.BuiltinType (demoted, BuiltinType (..), DemotedType (..))
import Control.Monad ((>>=))
import ZkFold.UPLC.Constant (Constant (..))
import Data.Bool (Bool(..))
import Prelude (error)
import ZkFold.UPLC.Data (Data (..), ConstructorTag)
import ZkFold.Data.Binary (toByteString, fromByteString, Binary)
import Numeric.Natural (Natural)
import Data.Text.Encoding (decodeUtf8)
import ZkFold.UPLC.BuiltinFunction
import ZkFold.Algebra.EllipticCurve.Class (decompress, Compressed, Compressible)
import Data.Maybe (fromJust)
import Data.String (String)
import Data.Either (Either)
import Data.Void (Void)
import qualified Data.ByteString as Strict

type CustomError = Void
type Stream = ByteString
type Parser = P.Parsec CustomError Stream
type ParseError = P.ParseErrorBundle Stream CustomError

parseProgram :: String -> ByteString -> Either ParseError VersionedProgram
parseProgram = P.runParser programParser

programParser :: Parser VersionedProgram
programParser =
  P.between (B.space <* sym "(" <* sym "program") (sym ")" *> P.eof) $
    Program <$> version <*> termIn []
 where
  version =
    (,,) <$> L.decimal <* B.string "." <*> L.decimal <* B.string "." <*> L.decimal

  termIn ctx =
    parens (asum
      [ sym "builtin" *> asum [ bTerm <$ sym bName | (bName, bTerm) <- builtins ]
      , TCase <$ sym "case" <*> termIn ctx <*> P.many (termIn ctx)
      , TConstr <$ sym "constr" <*> tag <*> P.many (termIn ctx)
      , sym "con" *> btype >>= demoted (fmap TConstant . constant)
      , TDelay <$ sym "delay" <*> termIn ctx
      , TError <$ sym "error"
      , TForce <$ sym "force" <*> termIn ctx
      , TLam <$ sym "lam" <*> do
          (pack -> x) <- P.some B.alphaNumChar
          termIn (x:ctx)
      ])
    <|> brackets (foldl1' TApp <$> P.some (termIn ctx))
    <|> asum [ TVariable ix <$ sym var | (ix, var) <- zip [0..] ctx ]

  constant :: DemotedType t -> Parser (Constant t)
  constant = \case
    DInteger -> CInteger <$> L.decimal
    DByteString -> CByteString <$> bsLiteral
    DString -> CString . decodeUtf8 . Strict.pack <$> P.between (sym "\"") (sym "\"")
        ((P.optional (sym "\\") *> P.anySingle) `P.manyTill` sym "\"")
    DBool -> CBool True <$ sym "True" <|> CBool False <$ sym "False"
    DUnit -> CUnit () <$ sym "(" <* sym ")"
    DData -> CData <$> datum
    DemList t -> CList <$> brackets (constant t `P.sepEndBy` sym ",")
    DPair t u -> parens $ CPair <$> constant t <* sym "," <*> constant u
    DG1 -> CG1 <$> point
    DG2 -> CG2 <$> point
    DGR -> error "Parsing of BLS pairing result not supported"

  datum = asum
    [ DI <$ sym "I" <*> L.decimal
    , DB <$ sym "B" <*> bsLiteral
    , DList <$ sym "List" <*> parens (datum `P.sepEndBy` sym ",")
    , DMap <$ sym "Map" <*> parens
        (parens ((,) <$> datum <* sym "," <*> datum) `P.sepEndBy` sym ",")
    , DConstr <$ sym "Constr" <*> tag <*> parens (datum `P.sepEndBy` sym ",")
    ]

  btype = asum
    [ BTBLSG1 <$ sym "bls12_381_G1_element"
    , BTBLSG2 <$ sym "bls12_381_G2_element"
    , BTBool <$ sym "bool"
    , BTByteString <$ sym "bytestring"
    , BTData <$ sym "data"
    , BTInteger <$ sym "integer"
    , BTString <$ sym "string"
    , BTUnit <$ sym "unit"
    , parens $
       BTList <$ sym "list" <*> btype
       <|> BTPair <$ sym "pair" <*> btype <*> btype
    ]

  point :: (Compressible point, Binary (Compressed point)) => Parser point
  point = decompress . fromJust . fromByteString
    . toByteString @Natural <$ sym "0x" <*> L.hexadecimal
  tag :: Parser ConstructorTag
  tag = L.decimal
  bsLiteral :: Parser Strict.ByteString
  bsLiteral = toByteString @Natural <$ B.string "#" <*> L.hexadecimal
  sym = L.symbol B.space1
  brackets, parens :: Parser a -> Parser a
  brackets = P.between (sym "[") (sym "]")
  parens = P.between (sym "(") (sym ")")

builtins :: [(ByteString, Term)]
builtins =
  [ ("addInteger", TBuiltin $ BFMono $ BMFInteger AddInteger)
  , ("subtractInteger", TBuiltin $ BFMono $ BMFInteger SubtractInteger)
  , ("multiplyInteger", TBuiltin $ BFMono $ BMFInteger MultiplyInteger)
  , ("divideInteger", TBuiltin $ BFMono $ BMFInteger DivideInteger)
  , ("modInteger", TBuiltin $ BFMono $ BMFInteger ModInteger)
  , ("quotientInteger", TBuiltin $ BFMono $ BMFInteger QuotientInteger)
  , ("remainderInteger", TBuiltin $ BFMono $ BMFInteger RemainderInteger)
  , ("equalsInteger", TBuiltin $ BFMono $ BMFInteger EqualsInteger)
  , ("lessThanInteger", TBuiltin $ BFMono $ BMFInteger LessThanInteger)
  , ("lessThanEqualsInteger", TBuiltin $ BFMono $ BMFInteger LessThanEqualsInteger)
  , ("appendByteString", TBuiltin $ BFMono $ BMFByteString AppendByteString)
  , ("consByteString", TBuiltin $ BFMono $ BMFByteString ConsByteString)
  , ("sliceByteString", TBuiltin $ BFMono $ BMFByteString SliceByteString)
  , ("lengthOfByteString", TBuiltin $ BFMono $ BMFByteString LengthOfByteString)
  , ("indexByteString", TBuiltin $ BFMono $ BMFByteString IndexByteString)
  , ("equalsByteString", TBuiltin $ BFMono $ BMFByteString EqualsByteString)
  , ("lessThanByteString", TBuiltin $ BFMono $ BMFByteString LessThanByteString)
  , ("lessThanEqualsByteString", TBuiltin $ BFMono $ BMFByteString LessThanEqualsByteString)
  , ("appendString", TBuiltin $ BFMono $ BMFString AppendString)
  , ("equalsString", TBuiltin $ BFMono $ BMFString EqualsString)
  , ("encodeUtf8", TBuiltin $ BFMono $ BMFString EncodeUtf8)
  , ("decodeUtf8", TBuiltin $ BFMono $ BMFString DecodeUtf8)
  , ("sha2_256", TBuiltin $ BFMono $ BMFAlgorithm SHA2_256)
  , ("sha3_256", TBuiltin $ BFMono $ BMFAlgorithm SHA3_256)
  , ("blake2b_256", TBuiltin $ BFMono $ BMFAlgorithm Blake2b_256)
  , ("verifyEd25519Signature", TBuiltin $ BFMono $ BMFAlgorithm VerifyEd25519Signature)
  , ("ifThenElse", TBuiltin $ BFPoly IfThenElse)
  , ("chooseUnit", TBuiltin $ BFPoly ChooseUnit)
  , ("trace", TBuiltin $ BFPoly Trace)
  , ("fstPair", TBuiltin $ BFPoly FstPair)
  , ("sndPair", TBuiltin $ BFPoly SndPair)
  , ("chooseList", TBuiltin $ BFPoly $ BPFList ChooseList)
  , ("mkCons", TBuiltin $ BFPoly $ BPFList MkCons)
  , ("headList", TBuiltin $ BFPoly $ BPFList HeadList)
  , ("tailList", TBuiltin $ BFPoly $ BPFList TailList)
  , ("nullList", TBuiltin $ BFPoly $ BPFList NullList)
  , ("chooseData", TBuiltin $ BFPoly ChooseData)
  , ("constrData", TBuiltin $ BFMono $ BMFData ConstrData)
  , ("mapData", TBuiltin $ BFMono $ BMFData MapData)
  , ("listData", TBuiltin $ BFMono $ BMFData ListData)
  , ("iData", TBuiltin $ BFMono $ BMFData IData)
  , ("bData", TBuiltin $ BFMono $ BMFData BData)
  , ("unConstrData", TBuiltin $ BFMono $ BMFData UnConstrData)
  , ("unMapData", TBuiltin $ BFMono $ BMFData UnMapData)
  , ("unListData", TBuiltin $ BFMono $ BMFData UnListData)
  , ("unIData", TBuiltin $ BFMono $ BMFData UnIData)
  , ("unBData", TBuiltin $ BFMono $ BMFData UnBData)
  , ("equalsData", TBuiltin $ BFMono $ BMFData EqualsData)
  , ("mkPairData", TBuiltin $ BFMono $ BMFData MkPairData)
  , ("mkNilData", TBuiltin $ BFMono $ BMFData MkNilData)
  , ("mkNilPairData", TBuiltin $ BFMono $ BMFData MkNilPairData)
  , ("serialiseData", TBuiltin $ BFMono $ BMFData SerializeData)
  , ("verifyEcdsaSecp256k1Signature", TBuiltin $ BFMono $ BMFAlgorithm VerifyEcdsaSecp256k1Signature)
  , ("verifySchnorrSecp256k1Signature", TBuiltin $ BFMono $ BMFAlgorithm VerifySchnorrSecp256k1Signature)
  , ("blake2b_224", TBuiltin $ BFMono $ BMFAlgorithm Blake2b_224)
  , ("keccak_256", TBuiltin $ BFMono $ BMFAlgorithm Keccak_256)
  , ("integerToByteString", TBuiltin $ BFMono $ BMFInteger IntegerToByteString)
  , ("byteStringToInteger", TBuiltin $ BFMono $ BMFInteger ByteStringToInteger)
  , ("bls12_381_G1_add", TBuiltin $ BFMono $ BMFCurve $ BLS_G1 Bls12_381_G1_add)
  , ("bls12_381_G1_neg", TBuiltin $ BFMono $ BMFCurve $ BLS_G1 Bls12_381_G1_neg)
  , ("bls12_381_G1_scalarMul", TBuiltin $ BFMono $ BMFCurve $ BLS_G1 Bls12_381_G1_scalarMul)
  , ("bls12_381_G1_equal", TBuiltin $ BFMono $ BMFCurve $ BLS_G1 Bls12_381_G1_equal)
  , ("bls12_381_G1_hashToGroup", TBuiltin $ BFMono $ BMFCurve $ BLS_G1 Bls12_381_G1_hashToGroup)
  , ("bls12_381_G1_compress", TBuiltin $ BFMono $ BMFCurve $ BLS_G1 Bls12_381_G1_compress)
  , ("bls12_381_G1_uncompress", TBuiltin $ BFMono $ BMFCurve $ BLS_G1 Bls12_381_G1_uncompress)
  , ("bls12_381_G2_add", TBuiltin $ BFMono $ BMFCurve $ BLS_G2 Bls12_381_G2_add)
  , ("bls12_381_G2_neg", TBuiltin $ BFMono $ BMFCurve $ BLS_G2 Bls12_381_G2_neg)
  , ("bls12_381_G2_scalarMul", TBuiltin $ BFMono $ BMFCurve $ BLS_G2 Bls12_381_G2_scalarMul)
  , ("bls12_381_G2_equal", TBuiltin $ BFMono $ BMFCurve $ BLS_G2 Bls12_381_G2_equal)
  , ("bls12_381_G2_hashToGroup", TBuiltin $ BFMono $ BMFCurve $ BLS_G2 Bls12_381_G2_hashToGroup)
  , ("bls12_381_G2_compress", TBuiltin $ BFMono $ BMFCurve $ BLS_G2 Bls12_381_G2_compress)
  , ("bls12_381_G2_uncompress", TBuiltin $ BFMono $ BMFCurve $ BLS_G2 Bls12_381_G2_uncompress)
  , ("bls12_381_millerLoop", TBuiltin $ BFMono $ BMFCurve Bls12_381_millerLoop)
  , ("bls12_381_mulMlResult", TBuiltin $ BFMono $ BMFCurve Bls12_381_mulMlResult)
  , ("bls12_381_finalVerify", TBuiltin $ BFMono $ BMFCurve Bls12_381_finalVerify)
  , ("andByteString", TBuiltin $ BFMono $ BMFBitwise AndByteString)
  , ("orByteString", TBuiltin $ BFMono $ BMFBitwise OrByteString)
  , ("xorByteString", TBuiltin $ BFMono $ BMFBitwise XorByteString)
  , ("complementByteString", TBuiltin $ BFMono $ BMFBitwise ComplementByteString)
  , ("shiftByteString", TBuiltin $ BFMono $ BMFBitwise ShiftByteString)
  , ("rotateByteString", TBuiltin $ BFMono $ BMFBitwise RotateByteString)
  , ("countSetBits", TBuiltin $ BFMono $ BMFBitwise CountSetBits)
  , ("findFirstSetBit", TBuiltin $ BFMono $ BMFBitwise FindFirstSetBit)
  , ("readBit", TBuiltin $ BFMono $ BMFBitwise ReadBit)
  , ("writeBits", TBuiltin $ BFMono $ BMFBitwise WriteBits)
  , ("replicateByte", TBuiltin $ BFMono $ BMFBitwise ReplicateByte)
  , ("ripemd_160", TBuiltin $ BFMono $ BMFAlgorithm Ripemd_160)
  ]
