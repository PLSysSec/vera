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
import           Data.List

cppGenTests :: BenchTest
cppGenTests = benchTestGroup "CPP Gen tests"
              [ --cppGenTests
                cppNotTest
              , cppAddTest
              , cppSubTest
              ]

cppNotTest :: BenchTest
cppNotTest = benchTestCase "cpp not test" $ do
  r <- evalCodegen Nothing $ do
    
    class_ range
    define not
    compiled <- compileFunction not
    let prog = intercalate "\n" compiled
    liftIO $ writeFile "test/GenCPP/not.cpp" prog
    error "test"
  error "test1"

cppAddTest :: BenchTest
cppAddTest = benchTestCase "cpp add test" $ do
  r <- evalCodegen Nothing $ do
    
    class_ range
    define add
    compiled <- compileFunction add
    let prog = intercalate "\n" compiled
    liftIO $ writeFile "test/GenCPP/add.cpp" prog
    error "test"
  error "test1"

cppSubTest :: BenchTest
cppSubTest = benchTestCase "cpp sub test" $ do
  r <- evalCodegen Nothing $ do
    
    class_ range
    define sub
    compiled <- compileFunction sub
    let prog = intercalate "\n" compiled
    liftIO $ writeFile "test/GenCPP/sub.cpp" prog
    error "test"
  error "test1"
