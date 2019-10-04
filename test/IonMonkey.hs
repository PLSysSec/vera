module IonMonkey where
import           BenchUtils
import           Control.Monad.State.Strict (liftIO)
import qualified DSL.Typed                  as T
import           IonMonkey.Objects
import           IonMonkey.Operations
import           Prelude                    hiding (and, not)
import           Test.Tasty.HUnit

ionMonkeyTests :: BenchTest
ionMonkeyTests = benchTestGroup "Ion Monkey tests" [ andTest
                                                   , notTest
                                                   , lshTest
                                                   , rshTest
                                                   , urshTest
                                                   , lsh'Test
                                                   , rsh'Test
                                                   , ursh'Test
                                                   ]

andTest :: BenchTest
andTest = benchTestCase "and" $ do
  (c0, c1, c2, c3, c4) <- T.evalVerif Nothing $ do

    leftRange <- signedInputRange "left start range"
    rightRange <- signedInputRange "right start range"
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
    opRange <- signedInputRange "operand range"
    resultRange <- not opRange
    c0 <- verifyConsistent
    c1 <- verifySaneRange resultRange
    c2 <- verifyDefinedResult resultRange

    -- Make sure that the result range actually corresponds to the range of the operator
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

    shifteeRange <- signedInputRange "shiftee range"
    val <- T.newInputVar T.Signed "val"
    resultRange <- lsh shifteeRange val
    c0 <- verifyConsistent
    c1 <- verifySaneRange resultRange
    c2 <- verifyDefinedResult resultRange

    shiftee <- operandWithRange "shiftee" T.Signed shifteeRange
    result <- T.jsShl shiftee val
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

    shifteeRange <- signedInputRange "shiftee range"
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
  shifteeRange <- signedInputRange "shiftee range"
  -- int32_t c
  val <- T.newInputVar T.Signed "val"

  resultRange <- ursh shifteeRange val
  c0 <- verifyConsistent
  c1 <- verifySaneRange resultRange
  c2 <- verifyDefinedResult resultRange
  liftIO $ Verified @=? c0
  liftIO $ Verified @=? c1
  liftIO $ Verified @=? c2

  shiftee <- operandWithRange "shiftee" T.Signed shifteeRange
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

    lhs <- signedInputRange "range ov value to shift"
    rhs <- signedInputRange "shift by"
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

    leftRange <- signedInputRange "shitee range"
    rightRange <- signedInputRange "shifter range"
    resultRange <- rsh' leftRange rightRange
    c0 <- verifyConsistent
    c1 <- verifySaneRange resultRange
    c2 <- verifyDefinedResult resultRange

    left <- operandWithRange "value to shift" T.Signed leftRange
    right <- operandWithRange "shit by" T.Signed rightRange
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
    leftRange <- signedInputRange "shitee range"
    rightRange <- signedInputRange "shifter range"
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























