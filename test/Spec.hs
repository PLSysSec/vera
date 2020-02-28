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
import           Sanity
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "All tests" $ map getTest allTests

allTests :: [BenchTest]
allTests = [ brokenIntersectTest
           , brokenCeilTest
           , genIonMonkeyTests
           , jsTests100
           , cppTests100
           , sanityCheck
           , jsTests1000
           , cppTests1000
           ]


