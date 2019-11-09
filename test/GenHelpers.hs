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
genHelpersTests = benchTestGroup "Helpers" [ ranges
                                           , lowerInit
                                           , upperInit
                                           , onesTest
                                           , zerosTest
                                           ]

ranges :: BenchTest
ranges = benchTestCase "ranges" $ do
  r <- evalCodegen Nothing $ do
    class_ range
    define $ setLowerInit
    define $ setUpperInit
    define $ range3
    define $ range4
    define $ range6
    genBodySMT [ declare (c "range") "testRange"
               , declare (t Signed) "low"
               , declare (t Signed) "high"
               , declare (t Bool) "nz"
               , declare (t Unsigned16) "exp"
               , v "testRange" `assign` call "Range3" [ cast (n Signed 8) Signed64
                                                      , cast (n Signed 8) Signed64
                                                      , n Bool 1
                                                      ]
               , v "low" `assign`  (v "testRange" .->. "lower")
               , v "high" `assign` (v "testRange" .->. "upper")
               , v "nz" `assign` (v "testRange" .->. "canBeNegativeZero")
               , v "testRange" `assign` call "Range4" [ cast (n Signed 100) Signed64
                                                      , cast (n Signed 100) Signed64
                                                      , n Bool 1
                                                      , n Unsigned16 12
                                                      ]
               , v "low" `assign`  (v "testRange" .->. "lower")
               , v "high" `assign` (v "testRange" .->. "upper")
               , v "nz" `assign` (v "testRange" .->. "canBeNegativeZero")
               , v "exp" `assign` (v "testRange" .->. "maxExponent")
               , v "testRange" `assign` call "Range6" [ cast (n Signed 500) Signed64
                                                      , n Bool 1
                                                      , cast (n Signed 400) Signed64
                                                      , n Bool 1
                                                      , n Bool 1
                                                      , n Unsigned16 12
                                                      ]
               , v "low" `assign`  (v "testRange" .->. "lower")
               , v "high" `assign` (v "testRange" .->. "upper")
               , v "nz" `assign` (v "testRange" .->. "canBeNegativeZero")
               , v "exp" `assign` (v "testRange" .->. "maxExponent")
               , declare (t Bool) "hlb"
               , declare (t Bool) "hub"
               , v "hlb" `assign` (v "testRange" .->. "hasInt32LowerBound")
               , v "hub" `assign` (v "testRange" .->. "hasInt32UpperBound")
               ]
    runSolverOnSMT
  vtest r $ Map.fromList [ ("low_1", 8)
                         , ("high_1", 8)
                         , ("nz_1", 1)
                         , ("exp_1", 12)
                         , ("low_2", 100)
                         , ("high_2", 100)
                         , ("low_3", 500)
                         , ("high_3", 400)
                         , ("nz_3", 1)
                         , ("exp_2", 12)
                         , ("hlb_1", 1)
                         , ("hub_1", 1)
                         ]


lowerInit :: BenchTest
lowerInit = benchTestCase "lower init" $ do

  r <- evalCodegen Nothing $ do
    class_ range
    define $ setLowerInit
    genBodySMT [ declare (c "range") "testRange"
               , v "testRange" `assign` call "setLowerInit" [ cast (n Signed 0) Signed64
                                                            , v "testRange"
                                                            ]
               , v "testRange" `assign` call "setLowerInit" [ cast (n Signed 20) Signed64
                                                            , v "testRange"
                                                            ]
               , v "testRange" `assign` call "setLowerInit" [ cast (n Signed 2147483647) Signed64
                                                            , v "testRange"
                                                            ]
               , v "testRange" `assign` call "setLowerInit" [ cast (n Signed 4294967295) Signed64
                                                            , v "testRange"
                                                            ]
               ]
    runSolverOnSMT
  vtest r $ Map.fromList [ ("testRange_lower_1", 0)
                         , ("testRange_lower_2", 20)
                         , ("testRange_lower_3", 2147483647)
                         , ("testRange_lower_4", 4294967295)
                         ]

upperInit :: BenchTest
upperInit = benchTestCase "upper init" $ do

  r <- evalCodegen Nothing $ do
    class_ range
    define $ setUpperInit
    genBodySMT [ declare (c "range") "testRange"
               , v "testRange" `assign` call "setUpperInit" [ cast (n Signed 0) Signed64
                                                            , v "testRange"
                                                            ]
               , v "testRange" `assign` call "setUpperInit" [ cast (n Signed 20) Signed64
                                                            , v "testRange"
                                                            ]
               , v "testRange" `assign` call "setUpperInit" [ cast (n Signed 2147483647) Signed64
                                                            , v "testRange"
                                                            ]
               , v "testRange" `assign` call "setUpperInit" [ cast (n Signed 4294967295) Signed64
                                                            , v "testRange"
                                                            ]
               ]
    runSolverOnSMT
  vtest r $ Map.fromList [ ("testRange_upper_1", 0)
                         , ("testRange_upper_2", 20)
                         , ("testRange_upper_3", 2147483647)
                         , ("testRange_upper_4", 4294967295)
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
               , declare (t Unsigned) "r6"
               , (v "r1") `assign` call "countOnes" [n Unsigned 1] -- 1
               , (v "r2") `assign` call "countOnes" [n Unsigned 31] -- 5
               , (v "r3") `assign` call "countOnes" [n Unsigned 2863311530] -- 16
               , (v "r4") `assign` call "countOnes" [n Unsigned 0] -- 0
               , (v "r5") `assign` call "countOnes" [n Unsigned 4294967295] -- 32
               , (v "r6") `assign` call "countOnes" [n Unsigned 1879048194] -- 4
               ]
    runSolverOnSMT
  vtest r $ Map.fromList [ ("r1_1", 1)
                         , ("r2_1", 5)
                         , ("r3_1", 16)
                         , ("r4_1", 0)
                         , ("r5_1", 32)
                         , ("r6_1", 4)
                         ]


zerosTest :: BenchTest
zerosTest = benchTestCase "count zeros" $ do

  r <- evalCodegen Nothing $ do

    define countOnes
    define countLeadingZeroes
    genBodySMT [ declare (t Unsigned) "r1"
               , declare (t Unsigned) "r2"
               , declare (t Unsigned) "r3"
               , declare (t Unsigned) "r4"
               , declare (t Unsigned) "r5"
               , declare (t Unsigned) "r6"
               , (v "r1") `assign` call "countLeadingZeroes" [n Unsigned 1] -- 31
               , (v "r2") `assign` call "countLeadingZeroes" [n Unsigned 31] -- 27
               , (v "r3") `assign` call "countLeadingZeroes" [n Unsigned 2863311530] -- 0
               , (v "r4") `assign` call "countLeadingZeroes" [n Unsigned 0] -- 32
               , (v "r5") `assign` call "countLeadingZeroes" [n Unsigned 4294967295] -- 0
               , (v "r6") `assign` call "countLeadingZeroes" [n Unsigned 1879048194] -- 1
               ]
    runSolverOnSMT
  vtest r $ Map.fromList [ ("r1_1", 31)
                         , ("r2_1", 27)
                         , ("r3_1", 0)
                         , ("r4_1", 32)
                         , ("r5_1", 0)
                         , ("r6_1", 1)
                         ]

