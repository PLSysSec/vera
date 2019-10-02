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
                                                   , urshTest_UInt32Range
                                                   , lsh'Test
                                                   , rsh'Test
                                                   , ursh'Test
                                                   ]

andTest :: BenchTest
andTest = benchTestCase "and" $ error "and"
  -- (c1, c2, c3) <- D.evalVerif Nothing $ do

  --   leftRange <- newInputRange "left start range" D.i32
  --   rightRange <- newInputRange "right start range" D.i32
  --   resultRange <- and leftRange rightRange
  --   c1 <- verifySaneRange resultRange

  --   left <- operandWithRange "left" D.i32 leftRange
  --   right <- operandWithRange "right" D.i32 rightRange
  --   result <- D.and left right
  --   c2 <- verifyLowerBound result resultRange
  --   c3 <- verifyUpperBound result resultRange
  --   return (c1, c2, c3)

  -- Verified @=? c1
  -- Verified @=? c2
  -- Verified @=? c3

notTest :: BenchTest
notTest = benchTestCase "not" $ error "not"
  -- (internalCheck, check1, check2) <- D.evalVerif Nothing $ do

  --   -- Setup the result range and make sure lower < upper
  --   opRange <- newInputRange "operand start range" D.i32
  --   resultRange <- not opRange
  --   internalCheck <- verifySaneRange resultRange

  --   -- Make sure that the result range actually corresponds to the range of the operator
  --   op <- operandWithRange "op" D.i32 opRange
  --   result <- D.not op
  --   c1 <- verifyUpperBound result resultRange
  --   c2 <- verifyLowerBound result resultRange

  --   return (internalCheck, c1, c2)


  -- Verified @=? internalCheck
  -- Verified @=? check1
  -- Verified @=? check2

lshTest :: BenchTest
lshTest = benchTestCase "lsh" $ error "lsh"
  -- (c1, c2, c3) <- D.evalVerif Nothing $ do

  --   shifteeRange <- newInputRange "shiftee range" D.i32
  --   val <- D.i32v "val"
  --   resultRange <- lsh shifteeRange val
  --   c1 <- verifySaneRange resultRange

  --   shiftee <- operandWithRange "shiftee" D.i32 shifteeRange
  --   result <- D.jsSll32 shiftee val
  --   c2 <- verifyUpperBound result resultRange
  --   c3 <- verifyLowerBound result resultRange

  --   return (c1, c2, c3)

  -- Verified @=? c1

  -- Verified @=? c2
  -- Verified @=? c3

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


urshTest_UInt32Range  :: BenchTest
urshTest_UInt32Range = benchTestCase "ursh uint32" $ D.evalVerif Nothing $ error "q"

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
lsh'Test = benchTestCase "lsh'" $ error "lsh'"
  -- (c1, c2, c3) <- D.evalVerif Nothing $ do

  --   lhs <- newInputRange "range ov value to shift" D.i32
  --   rhs <- newInputRange "shift by" D.i32
  --   resultRange <- lsh' lhs rhs
  --   c1 <- verifySaneRange resultRange

  --   lhsOp <- operandWithRange "value to shift" D.i32 lhs
  --   rhsOp <- operandWithRange "shit by" D.i32 rhs
  --   result <- D.jsSll32 lhsOp rhsOp
  --   c2 <- verifyUpperBound result resultRange
  --   c3 <- verifyLowerBound result resultRange

  --   return (c1, c2, c3)

  -- Verified @=? c1
  -- Verified @=? c2
  -- Verified @=? c3

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























