module GenIonMonkey (genIonMonkeyTests) where

import           BenchUtils
import           Control.Monad.State.Strict          (liftIO)
import           DSL.DSL                             (SMTResult (..))
import           DSL.Typed                           (Type (..))
import           Generate.Lang
import           Generate.SMTGen
import           Generate.State
import           IonMonkeyGenerated.Bugs
import           IonMonkeyGenerated.Helpers
import           IonMonkeyGenerated.Objects
import           IonMonkeyGenerated.Operations
import           IonMonkeyGenerated.Verify
import           IonMonkeyGenerated.VerifyIndividual
import           Prelude                             hiding (abs, and, floor,
                                                      max, min, not, or)
import           Test.Tasty.HUnit
import           Utils

genIonMonkeyTests :: BenchTest
genIonMonkeyTests = benchTestGroup "Generated IonMonkey tests"
                    [ -- unionTest
                    -- intersectTest
                    --  brokenIntersectTest
                    --  addTests
                    -- , subTests
                    -- -- , mulTests
                      andTests
                    , orTests
                    , xorTests
                    , notTests
                    , lshTests
                    , rshTests
                    -- , urshTests
                    , lsh'Tests
                    , rsh'Tests
                    -- , ursh'Tests
                    -- , absTests
                    -- , minTests
                    -- , maxTests
                    -- , floorTests
                     -- ceilTests
--                    , signTests
                    ]

unionTest :: BenchTest
unionTest = benchTestCase "union" $ evalCodegen Nothing $ testUnion $ Set "union" union

intersectTest :: BenchTest
intersectTest = benchTestCase "intersect" $ evalCodegen Nothing $ testIntersection $ Set "intersect" intersect

brokenIntersectTest :: BenchTest
brokenIntersectTest = benchTestCase "broken intersect" $ evalCodegen Nothing $ testIntersection $ Set "intersect" brokenIntersect

addTests :: BenchTest
addTests = mkFloatTests "Add" $ Binary "add" add jsAdd

subTests :: BenchTest
subTests = mkFloatTests "Sub" $ Binary "sub" sub jsSub

mulTests :: BenchTest
mulTests = mkFloatTests "Mul" $ Binary "mul" mul jsMul

andTests :: BenchTest
andTests = mki32Tests "And" $ Binary "and" and jsAnd

orTests :: BenchTest
orTests = mki32Tests "Or" $ Binary "or" or jsOr

xorTests :: BenchTest
xorTests = mki32Tests "Xor" $ Binary "xor" xor jsXOr

notTests :: BenchTest
notTests = mki32Tests "Not" $ Unary "not" not jsNot

lshTests :: BenchTest
lshTests = mki32Tests "Lsh" $ Constant "lsh" lsh jsLsh

rshTests :: BenchTest
rshTests = mki32Tests "Rsh" $ Constant "rsh" rsh jsRsh

urshTests :: BenchTest
urshTests = mki32Tests "Ursh" $ Constant "ursh" ursh jsUrsh

lsh'Tests :: BenchTest
lsh'Tests = mki32Tests "Lsh'" $ Binary "lsh'" lsh' jsLsh

rsh'Tests :: BenchTest
rsh'Tests = mki32Tests "Rsh'" $ Binary "rsh'" rsh' jsRsh

ursh'Tests :: BenchTest
ursh'Tests = mki32Tests "Ursh'" $ Binary "ursh'" ursh' jsRsh

absTests :: BenchTest
absTests = mkFloatTests "abs" $ Unary "abs" abs jsAbs

minTests :: BenchTest
minTests = mkFloatTests "min" $ Binary "min" min jsMin

maxTests :: BenchTest
maxTests = mkFloatTests "max" $ Binary "max" max jsMax

floorTests :: BenchTest
floorTests = mkFloatTests "floor" $ Unary "floor" floor jsFloor

ceilTests :: BenchTest
ceilTests = mkFloatTests "ceil" $ Unary "ceil" ceil jsCeil

signTests :: BenchTest
signTests = mkFloatTests "sign" $ Unary "sign" sign jsSign


-- Union and intersection

-- oldBugTests :: BenchTest
-- oldBugTests = benchTestGroup "Old bugs" [ badModTest
--                                         , goodModTest
--                                         ]

-- unionIntersectTest :: BenchTest
-- unionIntersectTest = benchTestCase "union and intersection" $ evalCodegen Nothing $ verifyMetaUnion

-- Old bugs

-- badModTest :: BenchTest
-- badModTest = benchTestCase "badMod32" $ evalCodegen Nothing $ verifyFpFunction "badMod32" jsRem [badMod32, newInt32Range]

-- goodModTest :: BenchTest
-- goodModTest = benchTestCase "goodMod32" $ evalCodegen Nothing $ verifyFpFunction "goodMod32" jsRem [goodMod32, newInt32Range]

-- FP

