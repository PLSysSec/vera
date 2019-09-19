module IonMonkey where
import           BenchUtils
import qualified DSL               as D
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

ionMonkeyTests :: BenchTest
ionMonkeyTests = benchTestGroup "Ion Monkey tests" []

-- | IonMonkey's leftshift operation
-- https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#999
-- lshTest :: BenchTestCase
-- lshTest = undefined

lsh = do
  bs <- D.newBoolectorState Nothing
  result <- D.evalBoolector bs $ do
    -- Do the shift of the constant input
    c <- D.i32v "c"
    const <- D.i32c 31
    shift <- D.and c const

    -- Setup the current bounds on the range
    lower <- D.i32v "lower bound"
    upper <- D.i32v "upper bound"
    finalLower <- D.i32v "final lower bound"
    finalUpper <- D.i32v "final upper bound"
    D.slte lower upper >>= D.assert

    -- Do their if-checking:
    -- lower << shift << 1 >> shift >> 1 == lower &&
    -- upper << shift << 1 >> shift >> 1 == upper
    -- range is lower << shift, upper << shift
    t1 <- D.safeSll lower shift
    t2 <- D.i32c 1 >>= D.safeSll t1
    t3 <- D.safeSrl t2 shift
    t4 <- D.i32c 1 >>= D.safeSrl t3
    check1 <- D.eq t4 lower
    t5 <- D.safeSll upper shift
    t6 <- D.i32c 1 >>= D.safeSll t5
    t7 <- D.safeSll t6 shift
    t8 <- D.i32c 1 >>= D.safeSll t7
    check2 <- D.eq t8 upper

    D.sat
  print result






