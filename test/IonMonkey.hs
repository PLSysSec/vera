module IonMonkey where
import           BenchUtils
import           Control.Monad.State.Strict (liftIO)
import qualified DSL.Typed                  as T
import           IonMonkey.Objects
import           IonMonkey.Operations
import           IonMonkey.Verify
import           Prelude                    hiding (abs, and, floor, max, min,
                                             not, or)
import           Test.Tasty.HUnit

ionMonkeyTests :: BenchTest
ionMonkeyTests = benchTestGroup "Ion Monkey tests" [ fpAddTest
                                                   , fpMulTest
                                                   , fpSubTest
                                                   , addTest
                                                   , andTest
                                                   , notTest
                                                   , lshTest
                                                   , rshTest
                                                   , urshTest
                                                   , lsh'Test
                                                   , rsh'Test
                                                   , ursh'Test
                                                   , orTest
                                                   , xorTest
                                                   , fpMinTest
                                                   , fpMaxTest
                                                   , fpAbsTest
                                                   , fpSignTest
                                                   , fpFloorTest
                                                   , fpCeilTest
                                                   ]

fpAddTest :: BenchTest
fpAddTest = benchTestCase "fpadd" $ do

  (c0, c1) <- T.evalVerif Nothing $ do

    leftRange <- inputRange T.Double "left start range"
    rightRange <- inputRange T.Double "right start range"
    resultRange <- add leftRange rightRange
    c0 <- verifyConsistent

    left <- operandWithRange "left" T.Double leftRange
    right <- operandWithRange "right" T.Double rightRange
    result <- T.jsAdd left right

    c1 <- verifyInfNan result resultRange
    c2 <- verifyNegZero result resultRange

    return (c0, c2)

  Verified @=? c0
  Verified @=? c1

fpMulTest :: BenchTest
fpMulTest = benchTestCase "fpmul" $ do

  (c0) <- T.evalVerif Nothing $ do

    leftRange <- inputRange T.Double "left start range"
    rightRange <- inputRange T.Double "right start range"
    resultRange <- mul leftRange rightRange

    left <- operandWithRange "left" T.Double leftRange
    right <- operandWithRange "right" T.Double rightRange
    result <- T.jsMul left right

    c0 <- verifyNegZero result resultRange

    return (c0)

  Verified @=? c0

fpSubTest :: BenchTest
fpSubTest = benchTestCase "fpsub" $ do

  (c0) <- T.evalVerif Nothing $ do

    leftRange <- inputRange T.Double "left start range"
    rightRange <- inputRange T.Double "right start range"
    resultRange <- sub leftRange rightRange

    left <- operandWithRange "left" T.Double leftRange
    right <- operandWithRange "right" T.Double rightRange
    result <- T.jsSub left right

    c0 <- verifyNegZero result resultRange
    return c0

  Verified @=? c0

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#251
addTest :: BenchTest
addTest = benchTestCase "add" $ do
  -- For now, verify it over int 32
  (c0, c1, c2, c3, c4) <- T.evalVerif Nothing $ do

    leftRange <- inputRange T.Signed "left start range"
    rightRange <- inputRange T.Signed "right start range"
    resultRange <- add leftRange rightRange
    c0 <- verifyConsistent
    c1 <- verifySaneRange resultRange
    c2 <- verifyDefinedResult resultRange

    left <- operandWithRange "left" T.Signed leftRange
    right <- operandWithRange "right" T.Signed rightRange
    result <- T.jsAdd left right
    c3 <- verifyLowerBound result resultRange
    c4 <- verifyUpperBound result resultRange
    return (c0, c1, c2, c3, c4)

  Verified @=? c0
  Verified @=? c1
  Verified @=? c2
  Verified @=? c3
  Verified @=? c4

andTest :: BenchTest
andTest = benchTestCase "and" $ do
  (c0, c1, c2, c3, c4) <- T.evalVerif Nothing $ do

    leftRange <- inputRange T.Signed "left start range"
    rightRange <- inputRange T.Signed "right start range"
    resultRange <- and leftRange rightRange
    c0 <- verifyConsistent
    c1 <- verifySaneRange resultRange
    c2 <- verifyDefinedResult resultRange

    left <- operandWithRange "left" T.Signed leftRange
    right <- operandWithRange "right" T.Signed rightRange
    result <- T.jsAnd left right
    c3 <- verifyLowerBound result resultRange
    c4 <- verifyUpperBound result resultRange
    return (c0, c1, c2, c3, c4)

  Verified @=? c0
  Verified @=? c1
  Verified @=? c2
  Verified @=? c3
  Verified @=? c4

notTest :: BenchTest
notTest = benchTestCase "not" $ do
  (c0, c1, c2, c3, c4) <- T.evalVerif Nothing $ do

    -- Setup the result range and make sure lower < upper
    opRange <- inputRange T.Signed "operand range"
    resultRange <- not opRange
    c0 <- verifyConsistent
    c1 <- verifySaneRange resultRange
    c2 <- verifyDefinedResult resultRange

    -- -- Make sure that the result range actually corresponds to the range of the operator
    op <- operandWithRange "op" T.Signed opRange
    result <- T.jsNot op
    c3 <- verifyUpperBound result resultRange
    c4 <- verifyLowerBound result resultRange

    return (c0, c1, c2, c3, c4)

  Verified @=? c0
  Verified @=? c1
  Verified @=? c2
  Verified @=? c3
  Verified @=? c4

lshTest :: BenchTest
lshTest = benchTestCase "lsh" $ do
  (c0, c1, c2, c3, c4) <- T.evalVerif Nothing $ do

    shifteeRange <- inputRange T.Signed "shiftee range"
    val <- T.newInputVar T.Signed "val"
    resultRange <- lsh shifteeRange val
    c0 <- verifyConsistent

    shiftee <- operandWithRange "shiftee" T.Signed shifteeRange
    result <- T.jsShl shiftee val
    c1 <- verifySaneRange resultRange
    c2 <- verifyDefinedResult resultRange
    c3 <- verifyUpperBound result resultRange
    c4 <- verifyLowerBound result resultRange

    return (c0, c1, c2, c3, c4)

  Verified @=? c0
  Verified @=? c1
  Verified @=? c2
  Verified @=? c3
  Verified @=? c4


rshTest :: BenchTest
rshTest = benchTestCase "rsh" $ do
  (c0, c1, c2, c3, c4) <- T.evalVerif Nothing $ do

    shifteeRange <- inputRange T.Signed "shiftee range"
    val <- T.newInputVar T.Signed "val"
    resultRange <- rsh shifteeRange val
    c0 <- verifyConsistent
    c1 <- verifySaneRange resultRange
    c2 <- verifyDefinedResult resultRange

    shiftee <- operandWithRange "shiftee" T.Signed shifteeRange
    result <- T.jsShr shiftee val
    c3 <- verifyUpperBound result resultRange
    c4 <- verifyLowerBound result resultRange

    return (c0, c1, c2, c3, c4)

  Verified @=? c0
  Verified @=? c1
  Verified @=? c2
  Verified @=? c3
  Verified @=? c4

-- | This may not be right; the exponent bit may be saving the range
urshTest  :: BenchTest
urshTest = benchTestCase "ursh" $ T.evalVerif Nothing $ do

  -- "ursh's left operand is uint32, not int32, but for range
  -- analysis we currently approximate it as int32."
  shifteeRange <- inputRange T.Signed "shiftee range"
  -- int32_t c
  val <- T.newInputVar T.Signed "val"

  resultRange <- ursh shifteeRange val
  c0 <- verifyConsistent
  c1 <- verifySaneRange resultRange
  c2 <- verifyDefinedResult resultRange
  liftIO $ Verified @=? c0
  liftIO $ Verified @=? c1
  liftIO $ Verified @=? c2

  shiftee <- operandWithRange "shiftee" T.Unsigned shifteeRange
  -- I am not sure if we want this or not?
  castVal <- T.cppCast val T.Unsigned
  result <- T.jsUshr shiftee castVal

  c3 <- verifyUpperBound result resultRange
  liftIO $ Verified @=? c3

  c4 <- verifyLowerBound result resultRange
  liftIO $ Verified @=? c4

lsh'Test :: BenchTest
lsh'Test = benchTestCase "lsh'" $ do
  (c0, c1, c2, c3, c4) <- T.evalVerif Nothing $ do

    lhs <- inputRange T.Signed "range of value to shift"
    rhs <- inputRange T.Signed "shift by"
    resultRange <- lsh' lhs rhs
    c0 <- verifyConsistent
    c1 <- verifySaneRange resultRange
    c2 <- verifyDefinedResult resultRange

    lhsOp <- operandWithRange "value to shift" T.Signed lhs
    rhsOp <- operandWithRange "shit by" T.Signed rhs
    result <- T.jsShl lhsOp rhsOp
    c3 <- verifyUpperBound result resultRange
    c4 <- verifyLowerBound result resultRange

    return (c0, c1, c2, c3, c4)

  Verified @=? c0
  Verified @=? c1
  Verified @=? c2
  Verified @=? c3
  Verified @=? c4

rsh'Test :: BenchTest
rsh'Test = benchTestCase "rsh'" $ do
  (c0, c1, c2, c3, c4) <- T.evalVerif Nothing $ do

    leftRange <- inputRange T.Signed "shitee range"
    rightRange <- inputRange T.Signed "shifter range"
    resultRange <- rsh' leftRange rightRange
    c0 <- verifyConsistent
    c1 <- verifySaneRange resultRange
    c2 <- verifyDefinedResult resultRange

    left <- operandWithRange "shiftee" T.Signed leftRange
    right <- operandWithRange "shifter" T.Signed rightRange
    result <- T.jsShr left right

    c3 <- verifyUpperBound result resultRange
    c4 <- verifyLowerBound result resultRange

    return (c0, c1, c2, c3, c4)

  Verified @=? c0
  Verified @=? c1
  Verified @=? c2
  Verified @=? c3
  Verified @=? c4

ursh'Test :: BenchTest
ursh'Test = benchTestCase "ursh'" $ do
  (c0, c1, c2, c3, c4) <- T.evalVerif Nothing $ do

    -- "ursh's left operand is uint32, not int32, but for range analysis we
    -- currently approximate it as int32."
    -- MOZ_ASSERT(lhs->isInt32());
    -- MOZ_ASSERT(lhs->isInt32());
    leftRange <- inputRange T.Signed "shitee range"
    rightRange <- inputRange T.Signed "shifter range"
    resultRange <- ursh' leftRange rightRange
    c0 <- verifyConsistent
    c1 <- verifySaneRange resultRange
    c2 <- verifyDefinedResult resultRange

    left <- operandWithRange "value to shift" T.Unsigned leftRange
    right <- operandWithRange "shit by" T.Unsigned rightRange
    result <- T.jsUshr left right

    c3 <- verifyUpperBound result resultRange
    c4 <- verifyLowerBound result resultRange

    return (c0, c1, c2, c3, c4)

  Verified @=? c0
  Verified @=? c1
  Verified @=? c2
  Verified @=? c3
  Verified @=? c4

orTest :: BenchTest
orTest = benchTestCase "or" $ do
  (c0, c1, c2, c3, c4) <- T.evalVerif Nothing $ do

    leftRange <- inputRange T.Signed "left start range"
    rightRange <- inputRange T.Signed "right start range"
    resultRange <- or leftRange rightRange
    c0 <- verifyConsistent
    c1 <- verifySaneRange resultRange
    c2 <- verifyDefinedResult resultRange

    left <- operandWithRange "left" T.Signed leftRange
    right <- operandWithRange "right" T.Signed rightRange
    result <- T.jsOr left right
    c3 <- verifyLowerBound result resultRange
    c4 <- verifyUpperBound result resultRange
    return (c0, c1, c2, c3, c4)

  Verified @=? c0
  Verified @=? c1
  Verified @=? c2
  Verified @=? c3
  Verified @=? c4

xorTest :: BenchTest
xorTest = benchTestCase "xor" $ do
  (c0, c1, c2, c3, c4) <- T.evalVerif Nothing $ do

    leftRange <- inputRange T.Signed "left start range"
    rightRange <- inputRange T.Signed "right start range"
    resultRange <- xor leftRange rightRange
    c0 <- verifyConsistent
    c1 <- verifySaneRange resultRange
    c2 <- verifyDefinedResult resultRange

    left <- operandWithRange "left" T.Signed leftRange
    right <- operandWithRange "right" T.Signed rightRange
    result <- T.jsXor left right
    c3 <- verifyLowerBound result resultRange
    c4 <- verifyUpperBound result resultRange
    return (c0, c1, c2, c3, c4)

  Verified @=? c0
  Verified @=? c1
  Verified @=? c2
  Verified @=? c3
  Verified @=? c4

fpMinTest :: BenchTest
fpMinTest = benchTestCase "fpmin" $ do
  (c2) <- T.evalVerif Nothing $ do

    leftRange <- inputRange T.Double "left start range"
    rightRange <- inputRange T.Double "right start range"
    resultRange <- min leftRange rightRange

    left <- operandWithRange "left" T.Double leftRange
    right <- operandWithRange "right" T.Double rightRange
    result <- T.jsMin left right

    c2 <- verifyNegZero result resultRange
    return c2

  Verified @=? c2

fpMaxTest :: BenchTest
fpMaxTest = benchTestCase "fpmax" $ do
  (c2) <- T.evalVerif Nothing $ do

    leftRange <- inputRange T.Double "left start range"
    rightRange <- inputRange T.Double "right start range"
    resultRange <- max leftRange rightRange

    left <- operandWithRange "left" T.Double leftRange
    right <- operandWithRange "right" T.Double rightRange
    result <- T.jsMax left right

    c2 <- verifyNegZero result resultRange
    return c2

  Verified @=? c2

fpAbsTest :: BenchTest
fpAbsTest = benchTestCase "fpabs" $ do
  (c2) <- T.evalVerif Nothing $ do

    inputRange <- inputRange T.Double "left start range"
    resultRange <- abs inputRange

    input <- operandWithRange "input" T.Double inputRange
    result <- T.jsAbs input

    c2 <- verifyNegZero result resultRange
    return c2

  Verified @=? c2

fpSignTest :: BenchTest
fpSignTest = benchTestCase "fpsign" $ do
  (c2) <- T.evalVerif Nothing $ do

    inputRange <- inputRange T.Double "left start range"
    resultRange <- sign inputRange

    input <- operandWithRange "input" T.Double inputRange
    result <- T.jsSign input

    c2 <- verifyNegZero result resultRange
    return c2

  Verified @=? c2

fpFloorTest :: BenchTest
fpFloorTest = benchTestCase "fpfloor" $ do
  (c2) <- T.evalVerif Nothing $ do

    inputRange <- inputRange T.Double "left start range"
    resultRange <- floor inputRange

    input <- operandWithRange "input" T.Double inputRange
    result <- T.jsFloor input

    c2 <- verifyNegZero result resultRange
    return c2

  Verified @=? c2

fpCeilTest :: BenchTest
fpCeilTest = benchTestCase "fpceil" $ do

  (c2) <- T.evalVerif Nothing $ do

    inputRange <- inputRange T.Double "left start range"
    resultRange <- ceil inputRange

    input <- operandWithRange "input" T.Double inputRange
    result <- T.jsCeil input

    c2 <- verifyNegZero result resultRange
    return c2

  Verified @=? c2

