module Cpp (cppTests) where
import           BenchUtils
import           Control.Monad.State.Strict (liftIO)
import qualified Data.Map                   as M
import qualified DSL.Typed                  as T
import           IonMonkey.Objects
import           IonMonkey.Operations
import           Prelude                    hiding (and, not)
import           Test.Tasty.HUnit
import           Utils

cppTests :: BenchTest
cppTests = benchTestGroup "C++ tests" [ cppMinTest
                                      , cppMaxTest
                                      , cppCmpTest
                                      , cppShlTest
                                      , cppShrTest
                                      , fpTest
                                      ]

trueBit :: Integer
trueBit = -1

falseBit :: Integer
falseBit = 0

cppMinTest :: BenchTest
cppMinTest = benchTestCase "min test" $ do

  r <- T.evalVerif Nothing $ do

    -- Check that cppMin is aware of the sign for signed numbers
    one <- T.num 1
    minusOne <- T.num (-1)
    result <- T.int32 "result"
    min <- T.cppMin one minusOne
    T.vassign result min

    -- Check that cppMin does the right thing with unsigned numbers
    uMinusOne <- T.unum (-1)
    uOne <- T.unum 1
    umin <- T.cppMin uMinusOne uOne
    uresult <- T.uint32 "uresult"
    T.vassign uresult umin

    T.runSolver

  vtest r $ M.fromList [ ("result", -1)
                       , ("uresult", 1)
                       ]

cppMaxTest :: BenchTest
cppMaxTest = benchTestCase "max test" $ do

  r <- T.evalVerif Nothing $ do

    -- Check that cppMax is aware of the sign for signed numbers
    one <- T.num 1
    minusOne <- T.num (-1)
    result <- T.int32 "result"
    min <- T.cppMax one minusOne
    T.vassign result min

    -- Check that cppMin does the right thing with unsigned numbers
    uMinusOne <- T.unum (-1)
    uOne <- T.unum 1
    umin <- T.cppMax uMinusOne uOne
    uresult <- T.uint32 "uresult"
    T.vassign uresult umin

    T.runSolver

  vtest r $ M.fromList [ ("result", 1)
                       , ("uresult", -1)
                       ]

cppCmpTest :: BenchTest
cppCmpTest = benchTestCase "cmp test" $ do

  r <- T.evalVerif Nothing $ do

    -- Make sure it doesn't segfault on equality comparison
    five <- T.num 5
    T.cppEq five five >>= T.vassert

    -- Make sure it uses a signed comparison for two signed numbers
    one <- T.num 1
    minusOne <- T.num (-1)
    result1 <- T.bool "result1"
    result2 <- T.bool "result2"
    result3 <- T.bool "result3"
    result4 <- T.bool "result4"
    gt <- T.cppGt one minusOne
    gte <- T.cppGte one minusOne
    lt <- T.cppLt one minusOne
    lte <- T.cppLte one minusOne
    T.vassign result1 gt
    T.vassign result2 gte
    T.vassign result3 lt
    T.vassign result4 lte

    -- Make sure that it uses an unsigned comparison for an unsigned and signed,
    -- unsigned and unsigned
    uMinusOne <- T.unum (-1)
    uOne <- T.unum 1

    result5 <- T.bool "result5"
    result6 <- T.bool "result6"
    result7 <- T.bool "result7"
    result8 <- T.bool "result8"
    result9 <- T.bool "result9"
    result10 <- T.bool "result10"
    result11 <- T.bool "result11"
    result12 <- T.bool "result12"

    bgt <- T.cppGt one uMinusOne
    bgte <- T.cppGte one uMinusOne
    blt <- T.cppLt one uMinusOne
    blte <- T.cppLte one uMinusOne

    T.vassign result5 bgt
    T.vassign result6 bgte
    T.vassign result7 blt
    T.vassign result8 blte

    ugt <- T.cppGt uOne uMinusOne
    ugte <- T.cppGte uOne uMinusOne
    ult <- T.cppLt uOne uMinusOne
    ulte <- T.cppLte uOne uMinusOne

    T.vassign result9 ugt
    T.vassign result10 ugte
    T.vassign result11 ult
    T.vassign result12 ulte

    T.runSolver

  vtest r $ M.fromList [ ("result1", trueBit)
                       , ("result2", trueBit)
                       , ("result3", falseBit)
                       , ("result4", falseBit)
                       , ("result5", falseBit)
                       , ("result6", falseBit)
                       , ("result7", trueBit)
                       , ("result8", trueBit)
                       , ("result9", falseBit)
                       , ("result10", falseBit)
                       , ("result11", trueBit)
                       , ("result12", trueBit)
                       ]

cppShlTest :: BenchTest
cppShlTest = benchTestCase "shl test" $ do

  r <- T.evalVerif Nothing $ do

    -- Shift of a negative should be undefined
    minusOne <- T.num (-1)
    two <- T.num 2
    shift1 <- T.cppShiftLeft minusOne two

    result1 <- T.int32 "result1"
    T.vassign result1 shift1

    -- Shift of any bit off the left end of the var should be undefined
    one <- T.num 1
    thirtyThree <- T.num 33
    shift2 <- T.cppShiftLeft one thirtyThree

    result2 <- T.int32 "result2"
    T.vassign result2 shift2

    -- Shift of any bit wayyyyy of the left end of the var should be undefined
    fourHundred <- T.num 400
    shift3 <- T.cppShiftLeft one fourHundred

    result3 <- T.int32 "result3"
    T.vassign result3 shift3

    -- Shift by a negative should be undefined
    shift4 <- T.cppShiftLeft one minusOne
    result4 <- T.int32 "result4"
    T.vassign result4 shift4

    -- Shift of bits out of an unsigned should not be undef
    uone <- T.unum 1
    shift5 <- T.cppShiftLeft uone fourHundred
    result5 <- T.uint32 "result5"
    T.vassign result5 shift5

    -- A normal shift of the result of an undef operation should still be undef
    shift6 <- T.cppShiftLeft shift3 one
    result6 <- T.int32 "result6"
    T.vassign result6 shift6

    -- Shift of an unsigned by a negative should be undef
    shift7 <- T.cppShiftLeft uone minusOne
    result7 <- T.uint32 "result7"
    T.vassign result7 shift7

    -- A normal shift of the result of an unsigned undef op should still be undef
    shift8 <- T.cppShiftLeft shift7 uone
    result8 <- T.uint32 "result8"
    T.vassign result8 shift8

    T.runSolver

  vtest r $ M.fromList [ ("result1_undef", trueBit)
                       , ("result2_undef", trueBit)
                       , ("result3_undef", trueBit)
                       , ("result4_undef", trueBit)
                       , ("result5_undef", falseBit)
                       , ("result6_undef", trueBit)
                       , ("result7_undef", trueBit)
                       , ("result8_undef", trueBit)
                       ]

cppShrTest :: BenchTest
cppShrTest = benchTestCase "shr test" $ do

  r <- T.evalVerif Nothing $ do

    -- Shifting an unsigned by a negative should be undef
    minusOne <- T.num (-1)
    uOne <- T.unum 1
    shift1 <- T.cppShiftRight uOne minusOne
    result1 <- T.uint32 "result1"
    T.vassign result1 shift1

    -- Shifting a signed by a negative should be undef
    one <- T.num 1
    shift2 <- T.cppShiftRight one minusOne
    result2 <- T.int32 "result2"
    T.vassign result2 shift2

    -- Shifting an unsigned should result in a logical shift
    uMinusOne <- T.unum (-1)
    thirtyOne <- T.unum 31
    shift3 <- T.cppShiftRight uMinusOne thirtyOne
    result3 <- T.uint32 "result3"
    T.vassign shift3 result3

    -- Shifting a signed should result in an arithmetic shift
    shift4 <- T.cppShiftRight minusOne thirtyOne
    result4 <- T.int32 "result4"
    T.vassign shift4 result4

    -- Shifting an undef thing should result in an undef thing
    shift5 <- T.cppShiftRight shift2 one
    result5 <- T.int32 "result5"
    T.vassign shift5 result5

    T.runSolver

  vtest r $ M.fromList [ ("result1_undef", trueBit)
                       , ("result2_undef", trueBit)
                       , ("result3", 1)
                       , ("result4", -1)
                       , ("result5_undef", trueBit)
                       ]

fpTest :: BenchTest
fpTest = benchTestCase "fp test" $ do

  r <- T.evalVerif Nothing $ do

    v <- T.fp "fp var"
    c <- T.fpnum 5.6

    added <- T.jsAdd v c
    subbed <- T.jsSub v c
    mulled <- T.jsMul v c
    pi <- T.posInf
    ni <- T.negInf
    pz <- T.posZero
    nz <- T.negZero
    n <- T.nan

    t1 <- T.isInf pi
    t2 <- T.isInf ni
    t3 <- T.isNan n
    t4 <- T.isNeg ni
    t5 <- T.isNeg nz
    t6 <- T.isPos pi
    t7 <- T.isPos pz
    t8 <- T.isZero pz
    t9 <- T.isZero nz

    T.runSolver

  vtest r $ M.fromList []
