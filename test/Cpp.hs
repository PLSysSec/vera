module Cpp where
import           BenchUtils
import           Control.Monad.State.Strict (liftIO)
import qualified DSL.DSL                    as D
import           IonMonkey.Objects
import           IonMonkey.Operations
import           Prelude                    hiding (and, not)
import           Test.Tasty.HUnit

cppTests :: BenchTest
cppTests = benchTestGroup "C++ tests" []

-- cppMinTest :: BenchTest
-- cppMinTest = benchTestCase "Cpp min" $ do
--   (c1, c2, c3) <- D.evalVerif Nothing $ do

--     return (c1, c2, c3)

--   Verified @=? c1
--   Verified @=? c2
--   Verified @=? c3
