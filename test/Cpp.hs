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

    left <- T.int32 "left"
    right <- T.int32 "right"
    result <- T.cppMin left right
    D.runSolver

  vtest "cppMin" r $ M.fromList [ ("left", 0)
                                , ("right", 0)
                                ]

