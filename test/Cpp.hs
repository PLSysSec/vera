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
                                      ]

trueBit :: Integer
trueBit = -1

falseBit :: Integer
falseBit = 0

cppMinTest :: BenchTest
cppMinTest = benchTestCase "min test" $ do

  (r) <- D.evalVerif Nothing $ do

    -- Check that cppMin is aware of the sign for signed numbers
    left <- T.int32 "left"
    one <- T.num 1
    T.vassign left one

    right <- T.int32 "right"
    minusOne <- T.num (-1)
    T.vassign right minusOne

    result <- T.int32 "result"
    min <- T.cppMin left right
    T.vassign result min

    -- Check that cppMin does the right thing with unsigned numbers
    uleft <- T.uint32 "uleft"
    uright <- T.uint32 "uright"
    T.vassign uleft one
    T.vassign uright minusOne

    uresult <- T.uint32 "uresult"
    umin <- T.cppMin uleft uright
    T.vassign uresult umin

    D.runSolver

  vtest "cppMin" r $ M.fromList [ ("left", 1)
                                , ("right", -1)
                                , ("result", -1)
                                , ("uresult", 1)
                                ]

cppGtTest :: BenchTest
cppGtTest = benchTestCase "gt test" $ do

  (r) <- D.evalVerif Nothing $ do

    -- Make sure it uses a signed comparison for two signed numbers
    left <- T.int32 "left"
    one <- T.num 1
    T.vassign left one

    right <- T.int32 "right"
    minusOne <- T.num (-1)
    T.vassign right minusOne

    result1 <- T.bool "result1"
    result2 <- T.bool "result2"
    gt <- T.cppGt left right
    gte <- T.cppGte left right
    T.vassign result1 gt
    T.vassign result2 gte

    -- Make sure that it uses an unsigned comparison for an unsigned and signed,
    -- unsigned and unsigned
    uright <- T.uint32 "uright"
    uleft <- T.uint32 "uleft"
    minusOne <- T.num (-1)
    one <- T.num 1
    T.vassign uright minusOne
    T.vassign uleft one

    result3 <- T.bool "result3"
    result4 <- T.bool "result4"
    result5 <- T.bool "result5"
    result6 <- T.bool "result6"
    bgt <- T.cppGt left uright
    bgte <- T.cppGte left uright
    ugt <- T.cppGt uleft uright
    ugte <- T.cppGte uleft uright
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

    D.runSolver

  vtest "shl test" r $ M.fromList [ ("result1_undef", trueBit)
                                  , ("result2_undef", trueBit)
                                  , ("result3_undef", trueBit)
                                  , ("result4_undef", trueBit)
                                  , ("result5_undef", falseBit)
                                  , ("result6_undef", trueBit)
                                  ]
