module Bench.Statistics (TestSize (..), printHeader, printSizeStatistics, getCostsCek) where

import qualified Data.ByteString                                   as BS
import           Data.SatInt                                       (fromSatInt)
import qualified Flat
import           PlutusCore.Default                                (DefaultFun, DefaultUni)
import           PlutusCore.Evaluation.Machine.ExBudget            (ExBudget (..))
import qualified PlutusCore.Evaluation.Machine.ExBudgetingDefaults as PLC
import           PlutusCore.Evaluation.Machine.ExMemory            (ExCPU (..), ExMemory (..))
import           Prelude                                           (Double, Fractional (..), Integer, Integral,
                                                                    Num (..), Show (..), String, fromIntegral, ($))
import           System.IO                                         (Handle, IO)
import           Text.Printf                                       (hPrintf, printf)
import qualified UntypedPlutusCore                                 as UPLC
import qualified UntypedPlutusCore.Evaluation.Machine.Cek          as Cek

-- Protocol parameters (June 2023)

-- | This is the "maximum transaction size".
maxTxSize :: Integer
maxTxSize = 16_384

-- | This is the "cpu transaction steps".
maxTxExSteps :: Integer
maxTxExSteps = 10_000_000_000

-- | This is the "memory transaction capacity".
maxTxExMem :: Integer
maxTxExMem = 14_000_000

toAnonDeBruijnProg
    :: UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
    -> UPLC.Program UPLC.DeBruijn      DefaultUni DefaultFun ()
toAnonDeBruijnProg (UPLC.Program () ver body) =
    UPLC.Program () ver $ UPLC.termMapNames UPLC.unNameDeBruijn body

-- | Evaluate a script and return the CPU and memory costs (according to the cost model)
getCostsCek :: UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun () -> (Integer, Integer)
getCostsCek (UPLC.Program () _ prog) =
    case Cek.runCekDeBruijn PLC.defaultCekParametersForTesting Cek.tallying Cek.noEmitter prog of
      (_, Cek.TallyingSt _ budget, _) ->
          let ExBudget (ExCPU cpu) (ExMemory mem) = budget
          in (fromSatInt cpu, fromSatInt mem)

---------------- Printing tables of information about costs ----------------

data TestSize =
    NoSize
  | TestSize Integer

stringOfTestSize :: TestSize -> String
stringOfTestSize =
    \case
     NoSize     -> "-"
     TestSize n -> show n

-- Printing utilities
percentage :: (Integral a, Integral b) => a -> b -> Double
percentage a b =
    let a' = fromIntegral a :: Double
        b' = fromIntegral b :: Double
    in (a' * 100) / b'

percentTxt :: (Integral a, Integral b) => a -> b -> String
percentTxt a b = printf "(%.1f%%)" (percentage a b)

-- | Print a header to be followed by a list of size statistics.
printHeader :: Handle -> IO ()
printHeader h = do
  hPrintf h "    n     Script size             CPU usage               Memory usage\n"
  hPrintf h "  ----------------------------------------------------------------------\n"

-- | Evaluate a script and print out the serialised size and the CPU and memory
-- usage, both as absolute values and percentages of the maxima specified in the
-- protocol parameters.
printSizeStatistics
    :: Handle
    -> TestSize
    -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
    -> IO ()
printSizeStatistics h n script = do
    let serialised = Flat.flat (UPLC.UnrestrictedProgram $ toAnonDeBruijnProg script)
        size = BS.length serialised
        (cpu, mem) = getCostsCek script
    hPrintf h "  %3s %7d %8s %15d %8s %15d %8s \n"
           (stringOfTestSize n)
           size (percentTxt size maxTxSize)
           cpu  (percentTxt cpu  maxTxExSteps)
           mem  (percentTxt mem  maxTxExMem)
