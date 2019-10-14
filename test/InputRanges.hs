module InputRanges (inputTests) where
import           BenchUtils
import           Control.Monad.State.Strict (liftIO)
import qualified Data.Map                   as M
import qualified DSL.Typed                  as T
import           IonMonkey.Helpers
import           IonMonkey.Objects
import           IonMonkey.Operations
import           Prelude                    hiding (and, not)
import           Test.Tasty.HUnit
import           Utils

inputTests :: BenchTest
inputTests = benchTestGroup "Input range tests" [ nanInfFlag
                                                , negZeroFlag
                                                , rangeTest
                                                ]


nanInfFlag :: BenchTest
nanInfFlag = benchTestCase "nan flag for input ranges" $ do

  r <- T.evalVerif Nothing $ do

    testRange <- inputRange T.Double "test range"
    let exp = maxExponent testRange
    includesInfinityAndNan >>= T.vassign exp
    test <- operandWithRange "test" T.Double testRange
    one <- T.fpnum 1
    T.vassign test one

    T.runSolver

  unsatTest r

negZeroFlag :: BenchTest
negZeroFlag = benchTestCase "nan flag for input ranges" $ do

  r <- T.evalVerif Nothing $ do

    testRange <- inputRange T.Double "test range"
    let nz = canBeNegativeZero testRange
    test <- operandWithRange "test" T.Double testRange
    T.false >>= T.vassign nz
    T.isZero test >>= T.vassert
    T.isNeg test >>= T.vassert

    T.runSolver

  unsatTest r

rangeTest :: BenchTest
rangeTest = benchTestCase "correct input ranges for hasbounds" $ do

  r <- T.evalVerif Nothing $ do

    testRange <- inputRange T.Double "test range"
    let hasLower = hasInt32LowerBound testRange
        lower_   = lower testRange
    test <- operandWithRange "test" T.Double testRange

    T.true >>= T.vassign hasLower
    T.num 1 >>= T.vassign lower_
    T.fpnum 0 >>= T.vassign test

    T.runSolver

  unsatTest r

  r <- T.evalVerif Nothing $ do

    testRange <- inputRange T.Double "test range"
    let hasUpper = hasInt32LowerBound testRange
        upper_   = upper testRange
    test <- operandWithRange "test" T.Double testRange

    T.true >>= T.vassign hasUpper
    T.num 0 >>= T.vassign upper_
    T.fpnum 1 >>= T.vassign test

    T.runSolver

  unsatTest r
