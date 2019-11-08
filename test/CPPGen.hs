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

writeCompiled :: String -> [String] -> Codegen ()
writeCompiled fileName comp = do
  let prog = intercalate "\n" comp
  liftIO $ writeFile fileName prog

cppNotTest :: BenchTest
cppNotTest = benchTestCase "cpp not test" $ do
  r <- evalCodegen Nothing $ do
    
    class_ range
    define not
    compiled <- compileFunction not
    writeCompiled "test/GenCPP/not.cpp" compiled
    error "test"
  error "test1"

cppAddTest :: BenchTest
cppAddTest = benchTestCase "cpp add test" $ do
  r <- evalCodegen Nothing $ do
    
    class_ range
    define add
    compiled <- compileFunction add
    writeCompiled "test/GenCPP/add.cpp" compiled
    error "test"
  error "test1"

cppSubTest :: BenchTest
cppSubTest = benchTestCase "cpp sub test" $ do
  r <- evalCodegen Nothing $ do
    
    class_ range
    define sub
    compiled <- compileFunction sub
    writeCompiled "test/GenCPP/sub.cpp" compiled
    error "test"
  error "test1"
