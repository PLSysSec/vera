import           BenchUtils
import           Cpp
import           IonMonkey
import           Test.Tasty
import           V8

main :: IO ()
main = defaultMain $ testGroup "All tests" $ map getTest allTests

allTests :: [BenchTest]
allTests = [ ionMonkeyTests
           , v8Tests
           , cppTests
           ]

