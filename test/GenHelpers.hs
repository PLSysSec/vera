module GenHelpers where
import           BenchUtils
import           Control.Monad.State.Strict    (liftIO)
import qualified Data.Map                      as Map
import           DSL.DSL                       (SMTResult (..))
import           DSL.Typed                     (Type (..))
import           Generate.Lang
import           Generate.SMTGen
import           Generate.State
import           IonMonkeyGenerated.Helpers
import           IonMonkeyGenerated.Objects
import           IonMonkeyGenerated.Operations
import           IonMonkeyGenerated.Verify
import           Prelude                       hiding (abs, and, floor, max,
                                                min, not, or)
import           Test.Tasty.HUnit
import           Utils

genHelpersTests :: BenchTest
genHelpersTests = benchTestGroup "Helpers" [ onesTest
                                           ]

onesTest :: BenchTest
onesTest = benchTestCase "count ones" $ do

  r <- evalCodegen Nothing $ do

    define countOnes
    genBodySMT [ declare (t Unsigned) "r1"
               , declare (t Unsigned) "r2"
               , declare (t Unsigned) "r3"
               , declare (t Unsigned) "r4"
               , declare (t Unsigned) "r5"
               , (v "r1") `assign` call "countOnes" [n Unsigned 1] -- 1
               , (v "r2") `assign` call "countOnes" [n Unsigned 31] -- 5
               , (v "r3") `assign` call "countOnes" [n Unsigned 2863311530] -- 16
               , (v "r4") `assign` call "countOnes" [n Unsigned 0] -- 0
               , (v "r5") `assign` call "countOnes" [n Unsigned 4294967295] -- 32
               ]
    runSolverOnSMT
  vtest r $ Map.fromList [ ("r1_1", 1)
                         , ("r2_1", 5)
                         , ("r3_1", 16)
                         , ("r4_1", 0)
                         , ("r5_1", 32)
                         ]


zeroesTest = error "NYI"
