import           BenchUtils
import           Buggy
import           Cpp
import           DSL
import           GenHelpers
import           GenIonMonkey
import           Helpers
import           InputRanges
import           IonMonkey
import           JavaScript
import           Lang
import           Parser
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "All tests" $ map getTest allTests

allTests :: [BenchTest]
allTests = [ brokenIntersectTest
           , brokenCeilTest
           , genIonMonkeyTests
           ]


