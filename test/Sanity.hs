module Sanity (sanityCheck) where
import           ActiveCode.JavaScript
import           BenchUtils
import           Control.Monad.State.Strict    (liftIO)
import           Cpp
import           DSL.DSL                       (SMTResult (..))
import           DSL.Typed                     (Type (..))
import           Generate.Lang
import           Generate.SMTGen
import           Generate.State
import           GenIonMonkey
import           IonMonkeyGenerated.Bugs
import           IonMonkeyGenerated.Helpers
import           IonMonkeyGenerated.Objects
import           IonMonkeyGenerated.Operations
import           IonMonkeyGenerated.Verify
import           JavaScript
import           Prelude                       hiding (abs, and, floor, max,
                                                min, not, or)
import           Test.Tasty.HUnit
import           Test.Tasty.HUnit
import           Utils
import           Utils

sanityCheck = benchTestGroup "Sanity" [ signTests
                                      , brokenCeilTest
                                      , jsBinOpTest JSAdd 10
                                      ]

brokenCeilTest :: BenchTest
brokenCeilTest = makeTest "ceil_sanity" $ testNegZ $ Unary "ceil" ceil jsCeil
  where makeTest str act = benchTestCase str $ evalCodegen Nothing act

