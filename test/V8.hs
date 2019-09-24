module V8 where
import           BenchUtils
import           Control.Monad.State.Strict (liftIO)
import qualified DSL.DSL                    as D
import           IonMonkey.Objects
import           Prelude                    hiding (and, not)
import           Test.Tasty.HUnit
import           V8.Operations

v8Tests :: BenchTest
v8Tests = benchTestGroup "V8 tests" [ andTest
                                    , rshTest
                                    ]

andTest :: BenchTest
andTest = benchTestCase "and" $ do
  (c1, c2, c3) <- D.evalVerif Nothing $ do

    leftRange <- newInputRange "left start range" D.i32
    rightRange <- newInputRange "right start range" D.i32
    resultRange <- numberBitwiseAnd leftRange rightRange
    c1 <- verifySaneRange resultRange

    left <- operandWithRange "left" D.i32 leftRange
    right <- operandWithRange "right" D.i32 rightRange
    result <- D.and left right
    c2 <- verifyLowerBound result resultRange
    c3 <- verifyUpperBound result resultRange
    return (c1, c2, c3)

  Verified @=? c1
  Verified @=? c2
  Verified @=? c3

rshTest :: BenchTest
rshTest = benchTestCase "rsh" $ do
  (c1, c2, c3) <- D.evalVerif Nothing $ do

    leftRange <- newInputRange "shitee range" D.i32
    rightRange <- newInputRange "shifter range" D.i32
    resultRange <- numberShiftRight leftRange rightRange
    c1 <- verifySaneRange resultRange

    left <- operandWithRange "value to shift" D.i32 leftRange
    right <- operandWithRange "shit by" D.i32 rightRange
    -- Need to mask https://www.ecma-international.org/ecma-262/5.1/#sec-11.7.3
    maskedRight <- D.i32c 31 >>= D.and right
    result <- D.safeSra left maskedRight

    c2 <- verifyUpperBound result resultRange
    c3 <- verifyLowerBound result resultRange

    return (c1, c2, c3)

  Verified @=? c1
  Verified @=? c2
  Verified @=? c3
