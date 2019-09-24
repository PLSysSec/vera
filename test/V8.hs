module V8 where
import           BenchUtils
import           Control.Monad.State.Strict (liftIO)
import qualified DSL.DSL                    as D
import           IonMonkey.Objects
import           Prelude                    hiding (and, not)
import           Test.Tasty.HUnit
import           V8.Operations

v8Tests :: BenchTest
v8Tests = benchTestGroup "V8 tests" [ andTest ]

andTest :: BenchTest
andTest = benchTestCase "and" $ do
  (c1, c2, c3) <- D.evalVerif Nothing $ do

    leftRange <- newInputRange "left start range" D.i32
    rightRange <- newInputRange "right start range" D.i32
    resultRange <- numberBitwiseAnd leftRange rightRange
    c1 <- verifySaneRange resultRange

    left <- operandWithRange "left" D.i32 leftRange
    right <- operandWithRange "right" D.i32 rightRange
    result <- D.and left right
    c2 <- verifyLowerBound result resultRange
    c3 <- verifyUpperBound result resultRange
    return (c1, c2, c3)

  Verified @=? c1
  Verified @=? c2
  Verified @=? c3


