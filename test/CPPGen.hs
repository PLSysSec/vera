module CPPGen (cppGenTests) where

import           BenchUtils
import           Control.Monad.State.Strict    (liftIO)
import           DSL.DSL                       (SMTResult (..))
import           DSL.Typed                     (Type (..))
import           Generate.Lang
import           Generate.SMTGen
import           Generate.State
import           Generate.CGen
import           IonMonkeyGenerated.Helpers
import           IonMonkeyGenerated.Objects
import           IonMonkeyGenerated.Operations
import           IonMonkeyGenerated.Verify
import           Prelude                       hiding (abs, and, floor, max,
                                                min, not, or)
import           Test.Tasty.HUnit
import           Utils

cppGenTests :: BenchTest
cppGenTests = benchTestGroup "CPP Gen tests"
              [ --cppGenTests
               cppAddTest
              ]

cppAddTest :: BenchTest
cppAddTest = benchTestCase "cpp add test" $ do
  r <- evalCodegen Nothing $ do
    
    class_ range
    compiled <- compileFunction not
    liftIO $ print compiled
    error "test"
  error "test1"
