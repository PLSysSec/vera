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
import           Utils

genIonMonkeyTests :: BenchTest
genIonMonkeyTests = benchTestGroup "Generated IonMonkey code test"
                    [ -- notTest
                     andTest
                    -- , rshTest
                    , rsh'Test
                    ]

notTest :: BenchTest
notTest = benchTestCase "not" $ do
  r <- evalCodegen Nothing $ do
    class_ range
    define newInt32InputRange
    define not
    define verifySaneRange
    let verif = [ declare (c "range") "not_range"
                , declare (c "range") "result_range"
                , (v "not_range") `assign` (call "newIn32InputRange" [])
                , (v "result_range") `assign` call "not" [v "not_range"]
                , vcall "verifySaneRange" [v "result_range"]
                ]
    genBodySMT verif
    runSolverOnSMT
  SolverUnsat @=? r

andTest :: BenchTest
andTest = benchTestCase "and" $
  evalCodegen Nothing $ verifyFunction "and" jsAnd [newInt32Range, and]

rshTest :: BenchTest
rshTest = benchTestCase "rsh" $ do
  r <- evalCodegen Nothing $ do
    class_ range
    define newInt32InputRange
    define newInt32Range
    define rsh
    define verifySaneRange
    let verif = [ declare (c "range") "left_range"
                , declare (t Signed) "right_range"
                , declare (c "range") "result_range"
                , (v "left_range") `assign` (call "newIn32InputRange" [])
                , (v "result_range") `assign` call "rsh" [v "left_range", v "right_range"]
                , vcall "verifySaneRange" [v "result_range"]
                ]
    genBodySMT verif
    runSolverOnSMT
  r @=? SolverUnsat

rsh'Test :: BenchTest
rsh'Test = benchTestCase "rsh'" $
  evalCodegen Nothing $ verifyFunction "rsh'" jsRsh [newInt32Range, rsh']
