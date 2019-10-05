module Helpers where
import           BenchUtils
import           Control.Monad.State.Strict (liftIO)
import qualified Data.Map                   as M
import qualified DSL.Typed                  as T
import           IonMonkey.Helpers
import           IonMonkey.Objects
import           IonMonkey.Operations
import           Prelude                    hiding (and, not, or)
import           Test.Tasty.HUnit
import           Utils

helpersTests :: BenchTest
helpersTests = benchTestGroup "Helpers tests" [ setRangeTest ]

setRangeTest = benchTestCase "set range" $ do
  r <- T.evalVerif Nothing $ do

    r1 <- resultRange T.Signed "result"
    l <- T.num64 9223372036854775807
    u <- T.num64 9223372036854775807
    f <- T.false
    n <- T.false
    e <- T.false
    setRange l u f n e r1

    T.runSolver

  vtest r $ M.fromList [ ("result_lower", 1)
                       , ("result_upper", 1)
                       ]


