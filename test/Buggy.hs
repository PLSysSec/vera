module Buggy (buggyTests) where
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

buggyTests = benchTestGroup "Bugs"
             [ brokenIntersectTest
             , brokenCeilTest
             ]

brokenIntersectTest :: BenchTest
brokenIntersectTest = benchTestCase "Broken intersect" $ evalCodegen Nothing $ testIntersection $ Set "intersect" brokenIntersect

brokenCeilTest :: BenchTest
brokenCeilTest = makeTest "Broken ceil" $ testNegZ $ Unary "ceil" ceil jsCeil
  where makeTest str act = benchTestCase str $ evalCodegen Nothing act
