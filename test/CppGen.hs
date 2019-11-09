module CppGen (cppGenTests) where

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
import           System.Directory              (createDirectoryIfMissing)

cppGenTests :: BenchTest
cppGenTests = benchTestGroup "CPP Gen tests"
              [ --cppGenTests
                cppNotTest
              , cppAddTest
              , cppSubTest
              , helloWorldTest
              ]

writeCompiled :: String -> [String] -> Codegen ()
writeCompiled fileName comp = do
  liftIO $ createDirectoryIfMissing True "test/GenCpp"
  let prog = intercalate "\n" comp
  liftIO $ writeFile fileName prog

cppNotTest :: BenchTest
cppNotTest = benchTestCase "cpp not test" $ do
  r <- evalCodegen Nothing $ do
    
    class_ range
    define not
    compiled <- compileFunction not
    writeCompiled "test/GenCpp/not.cpp" compiled
    error "test"
  error "test1"

cppAddTest :: BenchTest
cppAddTest = benchTestCase "cpp add test" $ do
  r <- evalCodegen Nothing $ do
    
    class_ range
    define add
    compiled <- compileFunction add
    writeCompiled "test/GenCpp/add.cpp" compiled
    error "test"
  error "test1"

cppSubTest :: BenchTest
cppSubTest = benchTestCase "cpp sub test" $ do
  r <- evalCodegen Nothing $ do
    
    class_ range
    define sub
    compiled <- compileFunction sub
    writeCompiled "test/GenCpp/sub.cpp" compiled
    error "test"
  error "test1"

helloWorldTest :: BenchTest
helloWorldTest = benchTestCase "hello world" $ do
  r <- evalCodegen Nothing $ do
    
    class_ range
    define add
    define sub
    define newInt32InputRange
    define canBeInfiniteOrNan
    define range3
    define range4
    define setLowerInit
    define setUpperInit
    rangeCompiled <- compileClass range
    range3Compiled <- compileFunction range3
    range4Compiled <- compileFunction range4
    lowerCompiled <- compileFunction setLowerInit
    upperCompiled <- compileFunction setUpperInit
    addCompiled <- compileFunction add
    subCompiled <- compileFunction sub
    newRangeCompiled <- compileFunction newInt32InputRange
    canBeInfCompiled <- compileFunction canBeInfiniteOrNan
    let compiled = concat [rangeCompiled, canBeInfCompiled, lowerCompiled, upperCompiled, range3Compiled, range4Compiled, addCompiled, subCompiled, newRangeCompiled]

    let header = "#include <iostream>\n\
    \#include <cassert>\n\n\
    \using namespace std;\n"

    let main = "int main() {\n\
      \range r1 = Range3(5, 10, false);\n\
      \range r2 = Range3(20, 25, false);\n\
      \range r3 = add(r1, r2);\n\
      \cout << \"upper bound: \" << r3.upper << endl;\n\
      \cout << \"lower bound: \" << r3.lower << endl;\n\
      \cout << \"has int32 lower bound: \" << r3.hasInt32LowerBound << endl;\n\
      \cout << \"has int32 upper bound: \" << r3.hasInt32UpperBound << endl;\n\
      \cout << \"can be negative zero: \" << r3.canBeNegativeZero << endl;\n\n\
      \return 0;\n\
    \}\n"

    let out = concat [[header], compiled, [main]]

    writeCompiled "test/GenCpp/hello_world.cpp" out

    error "test"
  error "test1"

