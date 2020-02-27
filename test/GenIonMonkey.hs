module GenIonMonkey ( genIonMonkeyTests
                    , signTests
                    ) where

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
genIonMonkeyTests = benchTestGroup "Verification"
                    [ -- unionTest
                    -- , intersectTest
                    -- , brokenIntersectTest
                     addTests
                    , subTests
                    , andTests
                    , orTests
                    , xorTests
                    , notTests
                    , mulTests
                    , lshTests
                    , rshTests
                    , urshTests
                    , lsh'Tests
                    , rsh'Tests
                    , ursh'Tests
                    , absTests
                    , minTests
                    , maxTests
                    , floorTests
                    , ceilTests
                    , signTests
                    ]

unionTest :: BenchTest
unionTest = benchTestCase "union_" $ evalCodegen Nothing $ testUnion $ Set "union_" union

intersectTest :: BenchTest
intersectTest = benchTestCase "intersect" $ evalCodegen Nothing $ testIntersection $ Set "intersect" intersect

brokenIntersectTest :: BenchTest
brokenIntersectTest = benchTestCase "broken intersect" $ evalCodegen Nothing $ testIntersection $ Set "intersect" brokenIntersect

addTests :: BenchTest
addTests = mkFloatTests "XX Add" "XXX Add" $ Binary "add" add jsAdd

subTests :: BenchTest
subTests = mkFloatTests "XX Sub" "XXX Sub" $ Binary "sub" sub jsSub

mulTests :: BenchTest
mulTests = mkFloatTests "XX Mul" "XXX Mul" $ Binary "mul" mul jsMul

andTests :: BenchTest
andTests = mki32Tests "XX And" "XXX And" $ Binary "and_" and jsAnd

orTests :: BenchTest
orTests = mki32Tests "XX Or" "XXX Or" $ Binary "or_" or jsOr

xorTests :: BenchTest
xorTests = mki32Tests "XX Xor" "XXX Xor" $ Binary "xor_" xor jsXOr

notTests :: BenchTest
notTests = mki32Tests "XX Not" "XXX Not" $ Unary "not_" not jsNot

lshTests :: BenchTest
lshTests = mki32Tests "XX Lsh" "XXX Lsh" $ Constant "lsh" lsh jsLsh

rshTests :: BenchTest
rshTests = mki32Tests "XX Rsh" "XXX Rsh" $ Constant "rsh" rsh jsRsh

urshTests :: BenchTest
urshTests = mki32Tests "XX Ursh" "XXX Ursh" $ Constant "ursh" ursh jsUrsh

lsh'Tests :: BenchTest
lsh'Tests = mki32Tests "XX Lsh'" "XXX Lsh'" $ Binary "lsh_p" lsh' jsLsh

rsh'Tests :: BenchTest
rsh'Tests = mki32Tests "XX Rsh'" "XXX Rsh'" $ Binary "rsh_p" rsh' jsRsh

ursh'Tests :: BenchTest
ursh'Tests = mki32Tests "XX Ursh'" "XXX Ursh'" $ Binary "ursh_p" ursh' jsRsh

absTests :: BenchTest
absTests = mkFloatTests "XX Abs" "XXX Abs" $ Unary "abs" abs jsAbs

minTests :: BenchTest
minTests = mkFloatTests "XX Min" "XXX Min" $ Binary "min" min jsMin

maxTests :: BenchTest
maxTests = mkFloatTests "XX Max" "XXX Max" $ Binary "max" max jsMax

floorTests :: BenchTest
floorTests = mkFloatTests "XX Floor" "XXX Floor" $ Unary "floor" floor jsFloor

ceilTests :: BenchTest
ceilTests = mkFloatTests "XX Ceil" "XXX Ceil" $ Unary "ceil" ceil jsCeil

signTests :: BenchTest
signTests = mkFloatTests "XX Sign" "XXX Sign" $ Unary "sign" sign jsSign


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

