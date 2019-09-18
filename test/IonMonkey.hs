module IonMonkey where
import           DSL
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

ionMonkeyTests :: BenchTest
ionMonkeyTests = benchTestGroup "Ion Monkey tests" []


