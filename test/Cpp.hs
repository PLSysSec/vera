module Cpp (cppTests) where
import           BenchUtils
import           Control.Monad.State.Strict (liftIO)
import qualified Data.Map                   as M
import qualified DSL.Cpp                    as T
import qualified DSL.DSL                    as D
import           IonMonkey.Objects
import           IonMonkey.Operations
import           Prelude                    hiding (and, not)
import           Test.Tasty.HUnit
import           Utils

cppTests :: BenchTest
cppTests = benchTestGroup "C++ tests" [ cppMinTest
                                      , cppGtTest
                                      ]

trueBit :: Integer
trueBit = -1

falseBit :: Integer
falseBit = 0

cppMinTest :: BenchTest
cppMinTest = benchTestCase "min test" $ do

  (r) <- D.evalVerif Nothing $ do

    -- Check that cppMin is aware of the sign for signed numbers
    left <- T.newSignedVar "left"
    one <- T.newSignedNumber 1
    T.vassign left one

    right <- T.newSignedVar "right"
    minusOne <- T.newSignedNumber (-1)
    T.vassign right minusOne

    result <- T.newSignedVar "result"
    min <- T.cppMin left right
    T.vassign result min

    -- Check that cppMin does the right thing with unsigned numbers
    uleft <- T.newUnsignedVar "uleft"
    uright <- T.newUnsignedVar "uright"
    T.vassign uleft one
    T.vassign uright minusOne

    uresult <- T.newUnsignedVar "uresult"
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
    left <- T.newSignedVar "left"
    one <- T.newSignedNumber 1
    T.vassign left one

    right <- T.newSignedVar "right"
    minusOne <- T.newSignedNumber (-1)
    T.vassign right minusOne

    result1 <- T.newBoolVar "result1"
    result2 <- T.newBoolVar "result2"
    gt <- T.cppGt left right
    gte <- T.cppGte left right
    T.vassign result1 gt
    T.vassign result2 gte

    -- Make sure that it uses an unsigned comparison for an unsigned and signed,
    -- unsigned and unsigned
    uright <- T.newUnsignedVar "uright"
    uleft <- T.newUnsignedVar "uleft"
    minusOne <- T.newUnsignedNumber (-1)
    one <- T.newUnsignedNumber 1
    T.vassign uright minusOne
    T.vassign uleft one

    result3 <- T.newBoolVar "result3"
    result4 <- T.newBoolVar "result4"
    result5 <- T.newBoolVar "result5"
    result6 <- T.newBoolVar "result6"
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
