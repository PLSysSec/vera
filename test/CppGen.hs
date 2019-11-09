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
import           Data.List                     (intercalate)
import           System.Directory              (createDirectoryIfMissing)

cppGenTests :: BenchTest
cppGenTests = benchTestGroup "CPP Gen tests"
              [ --cppGenTests
                cppNotTest
              , cppAddTest
              , cppSubTest
              , helloWorldTest
              , genAllFunctionsTest
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

genAllFunctionsTest :: BenchTest
genAllFunctionsTest = benchTestCase "cpp gen all functions test" $ do
  r <- evalCodegen Nothing $ do
    
    class_ range
    define add
    define sub
    define and
    define or
    define xor
    define not
    define mul
    define lsh
    define rsh
    define ursh
    define lsh'
    define rsh'
    define ursh'
    define abs
    define min
    define floor
    define ceil
    define sign
    define intersect
    define range3
    define range4
    define range6
    define newInt32Range
    define newUInt32Range
    define setLowerInit
    define setUpperInit
    define canHaveSignBitSet
    define exponentImpliedByInt32Bounds
    define hasInt32Bounds
    define numBits
    define canBeFiniteNonNegative
    define isFiniteNonNegative
    define isFiniteNegative
    define canBeInfiniteOrNan
    define missingAnyInt32Bounds
    define canBeNan
    define canBeZero
    define contains
    define countOnes
    define countLeadingZeroes
    define newInt32InputRange
    define newFloatInputRange
    define intInRange
    define floatInRange
    define verifySaneRange
    define verifyLower
    define verifyUpper
    define verifyNegZero
    define verifyNan
    define verifyInf
    define verifyExp

    compiledRange <- compileClass range
    compiledAdd <- compileFunction add
    compiledSub <- compileFunction sub
    compiledAnd <- compileFunction and
    compiledOr <- compileFunction or
    compiledXor <- compileFunction xor
    compiledNot <- compileFunction not
    compiledMul <- compileFunction mul
    compiledLsh <- compileFunction lsh
    compiledRsh <- compileFunction rsh
    compiledUrsh <- compileFunction ursh
    compiledLsh <- compileFunction lsh'
    compiledRsh <- compileFunction rsh'
    compiledUrsh <- compileFunction ursh'
    compiledAbs <- compileFunction abs
    compiledMin <- compileFunction min
    compiledFloor <- compileFunction floor
    compiledCeil <- compileFunction ceil
    compiledSign <- compileFunction sign
    compiledIntersect <- compileFunction intersect
    compiledRange3 <- compileFunction range3
    compiledRange4 <- compileFunction range4
    compiledRange6 <- compileFunction range6
    compiledNewInt32Range <- compileFunction newInt32Range
    compiledNewUInt32Range <- compileFunction newUInt32Range
    compiledSetLowerInit <- compileFunction setLowerInit
    compiledSetUpperInit <- compileFunction setUpperInit
    compiledCanHaveSignBitSet <- compileFunction canHaveSignBitSet
    compiledExponentImpliedByInt32Bounds <- compileFunction exponentImpliedByInt32Bounds
    compiledHasInt32Bounds <- compileFunction hasInt32Bounds
    compiledNumBits <- compileFunction numBits
    compiledCanBeFiniteNonNegative <- compileFunction canBeFiniteNonNegative
    compiledIsFiniteNonNegative <- compileFunction isFiniteNonNegative
    compiledIsFiniteNegative <- compileFunction isFiniteNegative
    compiledCanBeInfiniteOrNan <- compileFunction canBeInfiniteOrNan
    compiledMissingAnyInt32Bounds <- compileFunction missingAnyInt32Bounds
    compiledCanBeNan <- compileFunction canBeNan
    compiledCanBeZero <- compileFunction canBeZero
    compiledContains <- compileFunction contains
    compiledCountOnes <- compileFunction countOnes
    compiledCountLeadingZeroes <- compileFunction countLeadingZeroes
    compiledNewInt32InputRange <- compileFunction newInt32InputRange
    compiledNewFloatInputRange <- compileFunction newFloatInputRange
    compiledIntInRange <- compileFunction intInRange
    compiledFloatInRange <- compileFunction floatInRange
    compiledVerifySaneRange <- compileFunction verifySaneRange
    compiledVerifyLower <- compileFunction verifyLower
    compiledVerifyUpper <- compileFunction verifyUpper
    compiledVerifyNegZero <- compileFunction verifyNegZero
    compiledVerifyNan <- compileFunction verifyNan
    compiledVerifyInf <- compileFunction verifyInf
    compiledVerifyExp <- compileFunction verifyExp

    let compiled = concat [ compiledRange
                          , compiledAdd
                          , compiledSub
                          , compiledAnd
                          , compiledOr
                          , compiledXor
                          , compiledNot
                          , compiledMul
                          , compiledLsh
                          , compiledRsh
                          , compiledUrsh
                          , compiledLsh
                          , compiledRsh
                          , compiledUrsh
                          , compiledAbs
                          , compiledMin
                          , compiledFloor
                          , compiledCeil
                          , compiledSign
                          , compiledIntersect
                          , compiledRange3
                          , compiledRange4
                          , compiledRange6
                          , compiledNewInt32Range
                          , compiledNewUInt32Range
                          , compiledSetLowerInit
                          , compiledSetUpperInit
                          , compiledCanHaveSignBitSet
                          , compiledExponentImpliedByInt32Bounds
                          , compiledHasInt32Bounds
                          , compiledNumBits
                          , compiledCanBeFiniteNonNegative
                          , compiledIsFiniteNonNegative
                          , compiledIsFiniteNegative
                          , compiledCanBeInfiniteOrNan
                          , compiledMissingAnyInt32Bounds
                          , compiledCanBeNan
                          , compiledCanBeZero
                          , compiledContains
                          , compiledCountOnes
                          , compiledCountLeadingZeroes
                          , compiledNewInt32InputRange
                          , compiledNewFloatInputRange
                          , compiledIntInRange
                          , compiledFloatInRange
                          , compiledVerifySaneRange
                          , compiledVerifyLower
                          , compiledVerifyUpper
                          , compiledVerifyNegZero
                          , compiledVerifyNan
                          , compiledVerifyInf
                          , compiledVerifyExp]

    writeCompiled "test/GenCpp/all_functions.cpp" compiled
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

