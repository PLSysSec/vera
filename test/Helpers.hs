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
import           Test.Tasty.QuickCheck
import           Utils

helpersTests :: BenchTest
helpersTests = benchTestGroup "Helpers tests" [ setRangeTest
                                              , countLeadingZeroesTest 
                                              , countTrailingZeroesTest ]

trueBit :: Float
trueBit = 1

falseBit :: Float
falseBit = 0

setRangeTest = benchTestCase "set range" $ do
  r <- T.evalVerif Nothing $ do

    r1 <- resultRange T.Signed "r1"
    l1 <- T.num64 (-9223372036854775808)
    u1 <- T.num64 (-9223372036854775808)
    f <- T.false
    n <- T.false
    e <- T.unum16 6
    setRange l1 u1 f n e r1

    r2 <- resultRange T.Signed "r2"
    l2 <- T.num64 9223372036854775807
    u2 <- T.num64 9223372036854775807
    setRange l2 u2 f n e r2

    r3 <- resultRange T.Signed "r3"
    l3 <- T.num64 1
    u3 <- T.num64 5
    setRange l3 u3 f n e r3

    r4 <- resultRange T.Signed "r4"
    l4 <- T.num64 (-2147483648)
    u4 <- T.num64 (2147483647)
    setRange l4 u4 f n e r4

    r5 <- resultRange T.Signed "r5"
    l5 <- T.num64 (-9223372036854775)
    u5 <- T.num64 (-9223372036854775)
    setRange l5 u5 f n e r5

    T.runSolver

  vtest r $ M.fromList [ ("r1_lower", 2147483648.0)
                       , ("r1_upper", 2147483648)
                       , ("r1_hasLowerBound", falseBit)
                       , ("r1_hasUpperBound", trueBit)
                         --
                       , ("r2_lower", 2147483647)
                       , ("r2_upper", 2147483647)
                       , ("r2_hasLowerBound", trueBit)
                       , ("r2_hasUpperBound", falseBit)
                         --
                       , ("r3_lower", 1)
                       , ("r3_upper", 5)
                       , ("r3_hasLowerBound", trueBit)
                       , ("r3_hasUpperBound", trueBit)
                         --
                       , ("r4_lower", 2147483648)
                       , ("r4_upper", 2147483647)
                       , ("r4_hasLowerBound", trueBit)
                       , ("r4_hasUpperBound", trueBit)
                         --
                       , ("r5_lower", 2147483648)
                       , ("r5_upper", 2147483648)
                       , ("r5_hasLowerBound", falseBit)
                       , ("r5_hasUpperBound", trueBit)
                       ]

countLeadingZeroesTest :: BenchTest
countLeadingZeroesTest = benchTestCase "countLeadingZeroes" $ do
  r <- T.evalVerif Nothing $ do

    -- There should be no leading zeroes for a negative number
    minusOne <- T.num (-1)
    ctlz1 <- countLeadingZeroes32 minusOne
    result1 <- T.int32 "result1"
    T.vassign result1 ctlz1

    -- There should be 32 leading zeroes for 0
    zero <- T.num 0
    ctlz2 <- countLeadingZeroes32 zero
    result2 <- T.int32 "result2"
    T.vassign result2 ctlz2

    -- There should be 24 leading zeroes for 128
    num128 <- T.num 128
    ctlz3 <- countLeadingZeroes32 num128
    result3 <- T.int32 "result3"
    T.vassign result3 ctlz3

    T.runSolver

  vtest r $ M.fromList [ ("result1", 0)
                       , ("result2", 32)
                       , ("result3", 24)]

countTrailingZeroesTest :: BenchTest
countTrailingZeroesTest = benchTestCase "countTrailingZeroes" $ do
  r <- T.evalVerif Nothing $ do

    -- There shoud be no trailing zeroes for 1
    one <- T.num 1
    cttz1 <- countTrailingZeroes32 one
    result1 <- T.int32 "result1"
    T.vassign result1 cttz1

    -- There should be 32 trailing zeroes for 0
    zero <- T.num 0
    cttz2 <- countTrailingZeroes32 zero
    result2 <- T.int32 "result2"
    T.vassign result2 cttz2

    -- There should be 7 trailing zeroes for 128
    num128 <- T.num 128
    ctlz3 <- countTrailingZeroes32 num128
    result3 <- T.int32 "result3"
    T.vassign result3 ctlz3

    T.runSolver

  vtest r $ M.fromList [ ("result1", 0)
                       , ("result2", 32)
                       , ("result3", 7)]

-- propCtlz :: In32 -> 
propCtlz input32 = do
  (T.SolverSat vars) <- T.evalVerif Nothing $ do
    num <- T.num input
    T.named "result" $ countLeadingZeroes32 num
    T.runSolver
  let (Just val) = M.lookup "result" vars 
  assert $ fromInteger input == val

  where input = toInteger input32
