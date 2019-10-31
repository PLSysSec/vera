module GenIonMonkey (genIonMonkeyTests) where

import           BenchUtils
import           Control.Monad.State.Strict    (liftIO)
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

genIonMonkeyTests :: BenchTest
genIonMonkeyTests = benchTestGroup "Generated IonMonkey code test"
                    [ notTest ]

notTest :: BenchTest
notTest = benchTestCase "not" $ do
  r <- evalCodegen Nothing $ do
    class_ range
    define newInt32InputRange
    define not
    define verifySaneRange
    let verif = [ declare (c "range") "not_range"
                , declare (c "range") "result_range"
                , declare (t Signed) "dummy"
                , (v "not_range") `assign` (call "newIn32InputRange" [])
                , (v "result_range") `assign` call "not" [v "not_range"]
                , assign (v "dummy") $ call "verifySaneRange" [v "result_range"]
                ]
    genBodySMT verif
    runSolverOnSMT
  r @=? SolverUnsat

