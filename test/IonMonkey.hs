module IonMonkey where
import           BenchUtils
import           DSL
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

ionMonkeyTests :: BenchTest
ionMonkeyTests = benchTestGroup "Ion Monkey tests" []


