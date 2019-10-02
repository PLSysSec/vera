module Cpp (cppTests) where
import           BenchUtils
import           Control.Monad.State.Strict (liftIO)
import qualified Data.Map                   as M
import qualified DSL.DSL                    as D
import qualified DSL.Typed                  as T
import           IonMonkey.Objects
import           IonMonkey.Operations
import           Prelude                    hiding (and, not)
import           Test.Tasty.HUnit
import           Utils

cppTests :: BenchTest
cppTests = benchTestGroup "C++ tests" [ cppMinTest
                                      , cppGtTest
                                      , cppShlTest
                                      , cppShrTest
                                      ]

trueBit :: Integer
trueBit = -1

falseBit :: Integer
falseBit = 0

cppMinTest :: BenchTest
cppMinTest = benchTestCase "min test" $ do

  (r) <- D.evalVerif Nothing $ do

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

    D.runSolver

  vtest "cppMin" r $ M.fromList [ ("result", -1)
                                , ("uresult", 1)
                                ]

cppGtTest :: BenchTest
cppGtTest = benchTestCase "gt test" $ do

  (r) <- D.evalVerif Nothing $ do

    -- Make sure it uses a signed comparison for two signed numbers
    one <- T.num 1
    minusOne <- T.num (-1)
    result1 <- T.bool "result1"
    result2 <- T.bool "result2"
    gt <- T.cppGt one minusOne
    gte <- T.cppGte one minusOne
    T.vassign result1 gt
    T.vassign result2 gte

    -- Make sure that it uses an unsigned comparison for an unsigned and signed,
    -- unsigned and unsigned
    uMinusOne <- T.unum (-1)
    uOne <- T.unum 1

    result3 <- T.bool "result3"
    result4 <- T.bool "result4"
    result5 <- T.bool "result5"
    result6 <- T.bool "result6"

    bgt <- T.cppGt uOne uMinusOne
    bgte <- T.cppGte uOne uMinusOne
    ugt <- T.cppGt uOne minusOne
    ugte <- T.cppGte uOne minusOne

    T.vassign result3 bgt
    T.vassign result4 bgte
    T.vassign result5 ugt
    T.vassign result6 ugte

    D.runSolver

  vtest "cppMin" r $ M.fromList [ ("result1", trueBit)
                                , ("result2", trueBit)
                                , ("result3", falseBit)
                                , ("result4", falseBit)
                                , ("result5", falseBit)
                                , ("result6", falseBit)
                                ]

cppShlTest :: BenchTest
cppShlTest = benchTestCase "shl test" $ do

  (r) <- D.evalVerif Nothing $ do

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

    D.runSolver

  vtest "shl test" r $ M.fromList [ ("result1_undef", trueBit)
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

  r <- D.evalVerif Nothing $ do

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

    D.runSolver

  vtest "shr test" r $ M.fromList [ ("result1_undef", trueBit)
                                  , ("result2_undef", trueBit)
                                  , ("result3", 1)
                                  , ("result4", -1)
                                  , ("result5_undef", trueBit)
                                  ]
