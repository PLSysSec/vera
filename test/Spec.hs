import           BenchUtils
import           Cpp
import           DSL
import           Helpers
import           InputRanges
import           IonMonkey
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "All tests" $ map getTest allTests

allTests :: [BenchTest]
allTests = [ ionMonkeyTests
           , helpersTests
           , inputTests
           , cppTests
           , dslTests
           , dslTests
           ]

