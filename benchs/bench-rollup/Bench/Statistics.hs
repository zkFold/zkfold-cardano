module Bench.Statistics (getCostsCek) where

import           Data.SatInt                                       (fromSatInt)
import           PlutusCore.Default                                (DefaultFun, DefaultUni)
import           PlutusCore.Evaluation.Machine.ExBudget            (ExBudget (..))
import qualified PlutusCore.Evaluation.Machine.ExBudgetingDefaults as PLC
import           PlutusCore.Evaluation.Machine.ExMemory            (ExCPU (..), ExMemory (..))
import           Prelude                                           (Integer)
import qualified UntypedPlutusCore                                 as UPLC
import qualified UntypedPlutusCore.Evaluation.Machine.Cek          as Cek


-- Reference protocol parameters (June 2023):
-- maxTxExSteps = 10_000_000_000
-- maxTxExMem   = 14_000_000


-- | Evaluate a script and return the CPU and memory costs (according to the cost model)
getCostsCek :: UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun () -> (Integer, Integer)
getCostsCek (UPLC.Program () _ prog) =
    case Cek.runCekDeBruijn PLC.defaultCekParametersForTesting Cek.tallying Cek.noEmitter prog of
      (_, Cek.TallyingSt _ budget, _) ->
          let ExBudget (ExCPU cpu) (ExMemory mem) = budget
          in (fromSatInt cpu, fromSatInt mem)
