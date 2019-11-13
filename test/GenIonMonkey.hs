module GenIonMonkey (genIonMonkeyTests) where

import           BenchUtils
import           Control.Monad.State.Strict    (liftIO)
import           DSL.DSL                       (SMTResult (..))
import           DSL.Typed                     (Type (..))
import           Generate.Lang
import           Generate.SMTGen
import           Generate.State
import           IonMonkeyGenerated.Bugs
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
--                    oldBugTests
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
                   [ -- addTest
                  --   -- add32Test
                  -- , subTest
                  --  , sub32Test
                  -- -- , multest
                  -- --  , mul32Test
                    minTest
                   , min32Test
                   , maxTest
                   , max32Test
                   , floorTest
                   , floor32Test
                   , ceilTest
                   , ceil32Test
                   -- , absTest
                   , abs32Test
                   , signTest
                   , sign32Test
                   ]

oldBugTests :: BenchTest
oldBugTests = benchTestGroup "Old bugs" [ badModTest
                                        , goodModTest
                                        ]

-- Old bugs

badModTest :: BenchTest
badModTest = benchTestCase "badMod32" $ evalCodegen Nothing $ verifyFpFunction "badMod32" jsRem [badMod32, newInt32Range]

goodModTest :: BenchTest
goodModTest = benchTestCase "goodMod32" $ evalCodegen Nothing $ verifyFpFunction "goodMod32" jsRem [goodMod32, newInt32Range]

-- FP

addTest :: BenchTest
addTest = benchTestCase "add" $ evalCodegen Nothing $ verifyFpFunction "add" jsAdd [add]

add32Test :: BenchTest
add32Test = benchTestCase "add32" $ evalCodegen Nothing $ verifyFunction "add" jsAdd [add]

subTest :: BenchTest
subTest = benchTestCase "sub" $ evalCodegen Nothing $ verifyFpFunction "sub" jsSub [sub]

sub32Test :: BenchTest
sub32Test = benchTestCase "sub32" $ evalCodegen Nothing $ verifyFunction "sub" jsSub [sub]

mulTest :: BenchTest
mulTest = benchTestCase "mul" $ evalCodegen Nothing $ verifyFpFunction "mul" jsMul [mul]

mul32Test :: BenchTest
mul32Test = benchTestCase "mul32" $ evalCodegen Nothing $ verifyFunction "mul" jsMul [mul]

minTest :: BenchTest
minTest = benchTestCase "min" $ evalCodegen Nothing $ verifyFpFunction "min" jsMin [min]

min32Test :: BenchTest
min32Test = benchTestCase "min32" $ evalCodegen Nothing $ verifyFunction "min" jsMin [min]

maxTest :: BenchTest
maxTest = benchTestCase "max" $ evalCodegen Nothing $ verifyFpFunction "max" jsMax [max]

max32Test :: BenchTest
max32Test = benchTestCase "max32" $ evalCodegen Nothing $ verifyFunction "max" jsMax [max]

floorTest :: BenchTest
floorTest = benchTestCase "floor" $ evalCodegen Nothing $ verifyFpUnaryFunction "floor" jsFloor [floor]

floor32Test :: BenchTest
floor32Test = benchTestCase "floor32" $ evalCodegen Nothing $ verifyUnaryFunction "floor" jsFloor [floor]

ceilTest :: BenchTest
ceilTest = benchTestCase "ceil" $ evalCodegen Nothing $ verifyFpUnaryFunction "ceil" jsCeil [ceil]

ceil32Test :: BenchTest
ceil32Test = benchTestCase "ceil32" $ evalCodegen Nothing $ verifyUnaryFunction "ceil" jsCeil [ceil]

absTest :: BenchTest
absTest = benchTestCase "abs" $ evalCodegen Nothing $ verifyFpUnaryFunction "abs" jsAbs [abs]

abs32Test :: BenchTest
abs32Test = benchTestCase "abs32" $ evalCodegen Nothing $ verifyUnaryFunction "abs" jsAbs [abs]

signTest :: BenchTest
signTest = benchTestCase "sign" $ evalCodegen Nothing $ verifyFpUnaryFunction "sign" jsSign [sign]

sign32Test :: BenchTest
sign32Test = benchTestCase "sign32" $ evalCodegen Nothing $ verifyUnaryFunction "sign" jsSign [sign]

-- Int32

notTest :: BenchTest
notTest = benchTestCase "not" $ evalCodegen Nothing $ verifyUnaryFunction "not" jsNot [not, newInt32Range]

andTest :: BenchTest
andTest = benchTestCase "and" $
  evalCodegen Nothing $ verifyFunction "and" jsAnd [and, newInt32Range]

orTest :: BenchTest
orTest = benchTestCase "or" $
  evalCodegen Nothing $ verifyFunction "or" jsOr [or, newInt32Range, countLeadingZeroes, countOnes]

xorTest :: BenchTest
xorTest = benchTestCase "xor" $
  evalCodegen Nothing $ verifyFunction "xor" jsXOr [xor, newInt32Range, countLeadingZeroes, countOnes]

rshTest :: BenchTest
rshTest = benchTestCase "rsh" $
  evalCodegen Nothing $ verifyFunctionConstArg "rsh" jsRsh [rsh, newInt32Range]

urshTest :: BenchTest
urshTest = benchTestCase "ursh" $
  evalCodegen Nothing $ verifyFunctionConstArg "ursh" jsUrsh [ursh, newUInt32Range, isFiniteNonNegative, isFiniteNegative]

lshTest :: BenchTest
lshTest = benchTestCase "lsh" $
  evalCodegen Nothing $ verifyFunctionConstArg "lsh" jsLsh [lsh, newInt32Range]

rsh'Test :: BenchTest
rsh'Test = benchTestCase "rsh'" $
  evalCodegen Nothing $ verifyFunction "rsh'" jsRsh [rsh', newInt32Range]

ursh'Test :: BenchTest
ursh'Test = benchTestCase "ursh'" $
  evalCodegen Nothing $ verifyFunction "ursh'" jsUrsh [ursh', newUInt32Range]

lsh'Test :: BenchTest
lsh'Test = benchTestCase "lsh'" $
  evalCodegen Nothing $ verifyFunction "lsh'" jsRsh [lsh', newInt32Range]
