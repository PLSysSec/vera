module IonMonkey where
import           BenchUtils
import           Control.Monad.State.Strict (liftIO)
import qualified DSL.DSL                    as D
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
  (c1, c2, c3, c4) <- D.evalVerif Nothing $ do

    leftRange <- signedInputRange "left start range"
    rightRange <- signedInputRange "right start range"
    resultRange <- and leftRange rightRange
    c1 <- verifySaneRange resultRange
    c2 <- verifyDefinedResult resultRange

    left <- operandWithRange "left" T.Signed leftRange
    right <- operandWithRange "right" T.Signed rightRange
    result <- T.jsAnd left right
    c3 <- verifyLowerBound result resultRange
    c4 <- verifyUpperBound result resultRange
    return (c1, c2, c3, c4)

  Verified @=? c1
  Verified @=? c2
  Verified @=? c3
  Verified @=? c4

notTest :: BenchTest
notTest = benchTestCase "not" $ do
  (c1, c2, c3, c4) <- D.evalVerif Nothing $ do

    -- Setup the result range and make sure lower < upper
    opRange <- signedInputRange "operand range"
    resultRange <- not opRange
    c1 <- verifySaneRange resultRange
    c2 <- verifyDefinedResult resultRange

    -- Make sure that the result range actually corresponds to the range of the operator
    op <- operandWithRange "op" T.Signed opRange
    result <- T.jsNot op
    c3 <- verifyUpperBound result resultRange
    c4 <- verifyLowerBound result resultRange

    return (c1, c2, c3, c4)


  Verified @=? c1
  Verified @=? c2
  Verified @=? c3
  Verified @=? c4

lshTest :: BenchTest
lshTest = benchTestCase "lsh" $ do
  (c1, c2, c3, c4) <- D.evalVerif Nothing $ do

    shifteeRange <- signedInputRange "shiftee range"
    val <- T.newInputVar T.Signed "val"
    resultRange <- lsh shifteeRange val
    c1 <- verifySaneRange resultRange
    c2 <- verifyDefinedResult resultRange

    shiftee <- operandWithRange "shiftee" T.Signed shifteeRange
    result <- T.jsShl shiftee val
    c3 <- verifyUpperBound result resultRange
    c4 <- verifyLowerBound result resultRange

    return (c1, c2, c3, c4)

  Verified @=? c1
  Verified @=? c2
  Verified @=? c3
  Verified @=? c4


rshTest :: BenchTest
rshTest = benchTestCase "rsh" $ do
  (c1, c2, c3, c4) <- D.evalVerif Nothing $ do

    shifteeRange <- signedInputRange "shiftee range"
    val <- T.newInputVar T.Signed "val"
    resultRange <- rsh shifteeRange val
    c1 <- verifySaneRange resultRange
    c2 <- verifyDefinedResult resultRange

    shiftee <- operandWithRange "shiftee" T.Signed shifteeRange
    result <- T.jsShr shiftee val
    c3 <- verifyUpperBound result resultRange
    c4 <- verifyLowerBound result resultRange

    return (c1, c2, c3, c4)

  Verified @=? c1
  Verified @=? c2
  Verified @=? c3
  Verified @=? c4

-- | This may not be right; the exponent bit may be saving the range
urshTest  :: BenchTest
urshTest = benchTestCase "ursh" $ D.evalVerif Nothing $ error "ursh"

  -- shifteeRange <- newInputRange "shiftee range" D.i32
  -- val <- D.i32v "val"
  -- resultRange <- ursh shifteeRange val
  -- c1 <- verifySaneRange resultRange

  -- liftIO $ Verified @=? c1

  -- shiftee <- operandWithRange "shiftee" D.i32 shifteeRange
  -- result <- D.jsSrl32 shiftee val

  -- c2 <- verifyUpperBound result resultRange
  -- liftIO $ Verified @=? c2

  -- c3 <- verifyLowerBound result resultRange
  -- liftIO $ Verified @=? c3

lsh'Test :: BenchTest
lsh'Test = benchTestCase "lsh'" $ do
  (c1, c2, c3, c4) <- D.evalVerif Nothing $ do

    lhs <- signedInputRange "range ov value to shift"
    rhs <- signedInputRange "shift by"
    resultRange <- lsh' lhs rhs
    c1 <- verifySaneRange resultRange
    c2 <- verifyDefinedResult resultRange

    lhsOp <- operandWithRange "value to shift" T.Signed lhs
    rhsOp <- operandWithRange "shit by" T.Signed rhs
    result <- T.jsShl lhsOp rhsOp
    c3 <- verifyUpperBound result resultRange
    c4 <- verifyLowerBound result resultRange

    return (c1, c2, c3, c4)

  Verified @=? c1
  Verified @=? c2
  Verified @=? c3
  Verified @=? c4

rsh'Test :: BenchTest
rsh'Test = benchTestCase "rsh'" $ error "rsh'"
  -- (c1, c2, c3) <- D.evalVerif Nothing $ do

  --   leftRange <- newInputRange "shitee range" D.i32
  --   rightRange <- newInputRange "shifter range" D.i32
  --   resultRange <- rsh' leftRange rightRange
  --   c1 <- verifySaneRange resultRange

  --   left <- operandWithRange "value to shift" D.i32 leftRange
  --   right <- operandWithRange "shit by" D.i32 rightRange
  --   result <- D.jsSra32 left right

  --   c2 <- verifyUpperBound result resultRange
  --   c3 <- verifyLowerBound result resultRange

  --   return (c1, c2, c3)

  -- Verified @=? c1
  -- Verified @=? c2
  -- Verified @=? c3

ursh'Test :: BenchTest
ursh'Test = benchTestCase "ursh'" $ error "ursh"
  -- (c1, c2, c3) <- D.evalVerif Nothing $ do

  --   leftRange <- newInputRange "shitee range" D.i32
  --   rightRange <- newInputRange "shifter range" D.i32
  --   resultRange <- ursh' leftRange rightRange
  --   c1 <- verifySaneRange resultRange

  --   left <- operandWithRange "value to shift" D.i32 leftRange
  --   right <- operandWithRange "shit by" D.i32 rightRange
  --   result <- D.jsSra32 left right

  --   c2 <- verifyUpperBound result resultRange
  --   c3 <- verifyLowerBound result resultRange

  --   return (c1, c2, c3)

  -- Verified @=? c1
  -- Verified @=? c2
  -- Verified @=? c3























