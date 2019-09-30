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
                                    , orTest
                                    , rshTest
                                    , lshTest
                                    , urshTest
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

orTest :: BenchTest
orTest = benchTestCase "or" $ do
  (c1, c2, c3) <- D.evalVerif Nothing $ do

    leftRange <- newInputRange "left start range" D.i32
    rightRange <- newInputRange "right start range" D.i32
    resultRange <- numberBitwiseOr leftRange rightRange
    c1 <- verifySaneRange resultRange

    left <- operandWithRange "left" D.i32 leftRange
    right <- operandWithRange "right" D.i32 rightRange
    result <- D.or left right
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
    right <- operandWithRange "shift by" D.i32 rightRange
    result <- D.jsSra32 left right

    c2 <- verifyUpperBound result resultRange
    c3 <- verifyLowerBound result resultRange

    return (c1, c2, c3)

  Verified @=? c1
  Verified @=? c2
  Verified @=? c3

lshTest :: BenchTest
lshTest = benchTestCase "lsh" $ do
  (c1, c2, c3) <- D.evalVerif Nothing $ do

    shifteeRange <- newInputRange "shiftee range" D.i32
    shifterRange <- newInputRange "shifter range" D.i32
    resultRange <- numberShiftLeft shifteeRange shifterRange
    c1 <- verifySaneRange resultRange

    shiftee <- operandWithRange "shiftee" D.i32 shifteeRange
    shifter <- operandWithRange "shifter" D.i32 shifterRange
    result <- D.jsSll32 shiftee shifter
    c2 <- verifyUpperBound result resultRange
    c3 <- verifyLowerBound result resultRange

    return (c1, c2, c3)

  Verified @=? c1

  Verified @=? c2
  Verified @=? c3

urshTest :: BenchTest
urshTest = benchTestCase "ursh" $ do
  (c1, c2, c3) <- D.evalVerif Nothing $ do

    leftRange <- newInputRange "shitee range" D.i32
    rightRange <- newInputRange "shifter range" D.i32
    resultRange <- numberShiftRightLogical leftRange rightRange
    c1 <- uVerifySaneRange resultRange

    left <- operandWithRange "value to shift" D.i32 leftRange
    right <- operandWithRange "shit by" D.i32 rightRange
    result <- D.jsSrl32 left right

    c2 <- uVerifyUpperBound result resultRange
    c3 <- uVerifyLowerBound result resultRange

    return (c1, c2, c3)

  Verified @=? c1
  Verified @=? c2
  Verified @=? c3
