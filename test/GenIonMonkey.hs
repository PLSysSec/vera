module GenIonMonkey (genIonMonkeyTests) where

import           BenchUtils
import           Control.Monad.State.Strict    (liftIO)
import           DSL.DSL                       (SMTResult (..))
import           DSL.Typed                     (Type (..))
import           Generate.Lang
import           Generate.SMTGen
import           Generate.State
import           IonMonkeyGenerated.Helpers
import           IonMonkeyGenerated.Objects
import           IonMonkeyGenerated.Operations
import           IonMonkeyGenerated.Verify
import           Prelude                       hiding (abs, and, floor, max,
                                                min, not, or)
import           Test.Tasty.HUnit
import           Utils

genIonMonkeyTests :: BenchTest
genIonMonkeyTests = benchTestGroup "Generated IonMonkey tests"
                    [ --intIonMonkeyTests
                     fpIonMonkeyTests
                    ]

intIonMonkeyTests :: BenchTest
intIonMonkeyTests = benchTestGroup "Generated IonMonkey i32 tests"
                    [ notTest
                    , andTest
                    , orTest
                    , xorTest
                    , rshTest
                    , urshTest
                    , lshTest
                    , rsh'Test
                    , ursh'Test
                    , lsh'Test
                    ]

fpIonMonkeyTests :: BenchTest
fpIonMonkeyTests = benchTestGroup "Generated IonMonkey fp tests"
                   [ addTest
                   , add32Test
                   , subTest
                   , sub32Test
                   , mulTest
                  , mul32Test
                   , minTest
                   , maxTest
                   , floorTest
                   , ceilTest
                   , absTest
                   , signTest
                   ]


-- FP

addTest :: BenchTest
addTest = benchTestCase "add" $ evalCodegen Nothing $ verifyFpFunction "add" jsAdd [add]

add32Test :: BenchTest
add32Test = benchTestCase "add" $ evalCodegen Nothing $ verifyFunction "add" jsAdd [add]

subTest :: BenchTest
subTest = benchTestCase "sub" $ evalCodegen Nothing $ verifyFpFunction "sub" jsSub [sub]

sub32Test :: BenchTest
sub32Test = benchTestCase "sub" $ evalCodegen Nothing $ verifyFunction "sub" jsSub [sub]

mulTest :: BenchTest
mulTest = benchTestCase "mul" $ evalCodegen Nothing $ verifyFpFunction "mul" jsMul [mul]

mul32Test :: BenchTest
mul32Test = benchTestCase "mul" $ evalCodegen Nothing $ verifyFunction "mul" jsMul [mul]

minTest :: BenchTest
minTest = benchTestCase "min" $ evalCodegen Nothing $ verifyFpFunction "min" jsMin [min]

maxTest :: BenchTest
maxTest = benchTestCase "max" $ evalCodegen Nothing $ verifyFpFunction "max" jsMax [max]

floorTest :: BenchTest
floorTest = benchTestCase "floor" $ evalCodegen Nothing $ verifyFpUnaryFunction "floor" jsFloor [floor]

ceilTest :: BenchTest
ceilTest = benchTestCase "ceil" $ evalCodegen Nothing $ verifyFpUnaryFunction "ceil" jsCeil [ceil]

absTest :: BenchTest
absTest = benchTestCase "abs" $ evalCodegen Nothing $ verifyFpUnaryFunction "abs" jsAbs [abs]

signTest :: BenchTest
signTest = benchTestCase "sign" $ evalCodegen Nothing $ verifyFpUnaryFunction "sign" jsSign [sign]

-- Int32

notTest :: BenchTest
notTest = benchTestCase "not" $ evalCodegen Nothing $ verifyUnaryFunction "not" jsNot [not]

andTest :: BenchTest
andTest = benchTestCase "and" $
  evalCodegen Nothing $ verifyFunction "and" jsAnd [newInt32Range, and]

orTest :: BenchTest
orTest = benchTestCase "or" $
  evalCodegen Nothing $ verifyFunction "or" jsOr [newInt32Range, countOnes, countLeadingZeroes, or]

xorTest :: BenchTest
xorTest = benchTestCase "xor" $
  evalCodegen Nothing $ verifyFunction "xor" jsXOr [newInt32Range, countOnes, countLeadingZeroes, xor]

rshTest :: BenchTest
rshTest = benchTestCase "rsh" $
  evalCodegen Nothing $ verifyFunctionConstArg "rsh" jsRsh [newInt32Range, rsh]

urshTest :: BenchTest
urshTest = benchTestCase "ursh" $
  evalCodegen Nothing $ verifyFunctionConstArg "ursh" jsUrsh [newUInt32Range, isFiniteNegative, isFiniteNonNegative, ursh]

lshTest :: BenchTest
lshTest = benchTestCase "lsh" $
  evalCodegen Nothing $ verifyFunctionConstArg "lsh" jsLsh [newInt32Range, lsh]

rsh'Test :: BenchTest
rsh'Test = benchTestCase "rsh'" $
  evalCodegen Nothing $ verifyFunction "rsh'" jsRsh [newInt32Range, rsh']

ursh'Test :: BenchTest
ursh'Test = benchTestCase "ursh'" $
  evalCodegen Nothing $ verifyFunction "ursh'" jsUrsh [newUInt32Range, ursh', isFiniteNonNegative]

lsh'Test :: BenchTest
lsh'Test = benchTestCase "lsh'" $
  evalCodegen Nothing $ verifyFunction "lsh'" jsRsh [newInt32Range, lsh']
