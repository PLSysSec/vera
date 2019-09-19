module IonMonkey where
import           BenchUtils
import           Control.Monad.State.Strict    (liftIO)
import qualified DSL.DSL                       as D
import           IonMonkeyOps.IonMonkeyObjects
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

ionMonkeyTests :: BenchTest
ionMonkeyTests = benchTestGroup "Ion Monkey tests" [ lsh
                                                   , rsh
                                                   ]

-- | IonMonkey's left shift operation
-- https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#999
lsh :: BenchTest
lsh = benchTestCase "lsh" $ do
  bs <- D.newBoolectorState Nothing
  result <- D.evalBoolector bs $ do
    -- We are shifting by a constant, compute the shift amount
    c <- D.i32v "c"
    const <- D.i32c 31
    shift <- D.and c const

    -- Create the other input range and the output range
    startRange <- newRange "start range" D.i32
    endRange <- newRange "end range" D.i32

    -- Do their if-checking:
    -- lower << shift << 1 >> shift >> 1 == lower &&
    -- upper << shift << 1 >> shift >> 1 == upper
    -- range is lower << shift, upper << shift
    -- Lower
    t1 <- D.safeSll (lower startRange) shift
    t2 <- D.i32c 1 >>= D.safeSll t1
    t3 <- D.safeSrl t2 shift
    t4 <- D.i32c 1 >>= D.safeSrl t3
    check1 <- D.eq t4 (lower startRange)
    -- Upper
    t5 <- D.safeSll (upper startRange) shift
    t6 <- D.i32c 1 >>= D.safeSll t5
    t7 <- D.safeSll t6 shift
    t8 <- D.i32c 1 >>= D.safeSll t7
    check2 <- D.eq t8 (upper startRange)

    -- Set new range bounds
    defaultLower <- D.i32min
    defaultUpper <- D.i32max
    D.cond check1 t1 defaultLower >>= D.eq (lower endRange)
    D.cond check2 t5 defaultUpper >>= D.eq (upper endRange)

    -- Verify that the operation is always in bounds
    operand <- operandWithRange "shifted variable" D.i32 startRange
    shiftResult <- D.safeSll operand c
    verifyInRange shiftResult endRange
    D.sat

  result @=? D.Sat

-- | IonMonkey's right shift operation
-- https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1016
rsh :: BenchTest
rsh = benchTestCase "rsh" $ do
  bs <- D.newBoolectorState Nothing
  result <- D.evalBoolector bs $ do
    -- Compute the shift amount
    c <- D.i32v "c"
    const <- D.i32c 31
    shift <- D.and c const

    -- Setup the current bounds on the range
    lower <- D.i32v "lower bound"
    upper <- D.i32v "upper bound"
    finalLower <- D.i32v "final lower bound"
    finalUpper <- D.i32v "final upper bound"
    D.slte lower upper >>= D.assert
    D.slte lower upper >>= D.assert

    -- Calculate the new range
    D.safeSrl lower shift >>= D.eq finalLower
    D.safeSrl upper shift >>= D.eq finalUpper

    -- Verify that the operation is always in bounds
    operand <- D.i32v "the shifted variable"
    D.slte operand upper >>= D.assert
    D.sgte operand lower >>= D.assert
    shiftResult <- D.safeSrl operand c
    D.slte shiftResult finalUpper >>= D.assert
    D.sgte shiftResult finalLower >>= D.assert
    D.sat

  result @=? D.Sat

