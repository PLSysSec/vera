import           BenchUtils
import           Cpp
import           DSL
import           Helpers
import           InputRanges
import           IonMonkey
import           JavaScript
import           Lang
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "All tests" $ map getTest allTests

allTests :: [BenchTest]
allTests = [ -- ionMonkeyTests
           -- , notTerminating
           -- , helpersTests
           -- , inputTests
           -- , cppTests
           -- , jsTests
           -- , dslTests
           langTests
           ]
