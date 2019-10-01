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
cppTests = benchTestGroup "C++ tests" [ cppMinTest ]

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

