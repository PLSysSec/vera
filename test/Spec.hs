import           BenchUtils
import           Cpp
import           CppGen
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
allTests = [ -- ionMonkeyTests
           -- , notterminating
           -- , helpersTests
            genIonMonkeyTests
           -- , inputTests
--            cppTests
           -- , jsTests
--           dslTests
           --, langTests
           , parserTests
           , cppGenTests
           ]
