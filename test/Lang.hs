module Lang where
import           BenchUtils
import qualified Data.Map        as Map
import           DSL.Typed       (Type (..))
import           Generate.Lang
import           Generate.SMTGen
import           Generate.State
import           Utils

langTests :: BenchTest
langTests = benchTestGroup "Lang" [ declTest
                                  , ifTest
                                  , callTest
                                  -- , returnTest
                                  ]

declTest :: BenchTest
declTest = benchTestCase "decl" $ do

  r <- evalCodegen Nothing $ do

    let decls = [ declare (t Signed) "x"
                , declare (t Signed) "y"
                , (v "x") `assign` (ne Signed 5)
                , (v "x") `assign` (ne Signed 7)
                , (v "y") `assign` (ne Signed 10)
                , (v "x") `assign` (ne Signed 9)
                ]
    genBodySMT decls
    runSolverOnSMT
  vtest r $ Map.fromList [ ("x_1", 5)
                         , ("x_2", 7)
                         , ("x_3", 9)
                         , ("y_1", 10)
                         ]

ifTest :: BenchTest
ifTest = benchTestCase "if" $ do

  r <- evalCodegen Nothing $ do

    let decls = [ declare (t Signed) "x"
                , declare (t Signed) "y"
                , declare (t Signed) "z"
                , (v "x") `assign` (ne Signed 5)
                , (v "y") `assign` (ne Signed 10)
                , (v "z") `assign` (ne Signed 20)
                , if_ ((ve "y") .<. (ne Signed 11)) [ v "x" `assign` (ne Signed 8)
                                                    , v "x" `assign` (ne Signed 20)
                                                    ] [v "x" `assign` (ne Signed 12)]
                , if_ ((ve "x") .<. (ne Signed 4)) [ v "z" `assign` (ne Signed 0) ] []
                ]
    genBodySMT decls
    runSolverOnSMT
  vtest r $ Map.fromList [ ("x_1", 5)
                         , ("y_1", 10)
                         , ("z_1", 20)
                         , ("x_2", 8)
                         , ("x_3", 20)
                         , ("z_2", 20)
                         ]

callTest :: BenchTest
callTest = benchTestCase "call" $ do

  r <- evalCodegen Nothing $ do

    let args = [("x", t Signed), ("y", t Signed)]
        body = [ declare (t Signed) "ret"
               , (v "ret") `assign` (ve "x" .+. ve "y")
               , return_ (ve "ret")
               ]
    define "fun" (t Signed) args body
    genBodySMT [ declare (t Signed) "result"
               , (v "result") `assign` call "fun" [ne Signed 5, ne Signed 10]
               ]
    genBodySMT [ declare (t Signed) "result2"
               , (v "result2") `assign` call "fun" [ne Signed 2, ne Signed 4]
               ]
    runSolverOnSMT
  vtest r $ Map.fromList [ ("result_1", 15)
                         , ("result2_1", 6)
                         ]

-- returnTest :: BenchTest
-- returnTest = benchTestCase "return" $ do

--   r <- evalCodegen Nothing $ do

--     let args = [("x", Signed)]
--         body = [ if_ ((v "x") .<. (number Signed 4)) [ returnFrom "fun" (number Signed 4) ] (Just [ returnFrom "fun" (number Signed 8) ])
--                ]
--     genFunctionSMT $ define "fun" Signed args body
--     genBodySMT [ declare Signed "result"
--                , (v "result") `assign` call "fun" [number Signed 3]
--                ]
--     genBodySMT [ declare Signed "result2"
--                , (v "result2") `assign` call "fun" [number Signed 5]
--                ]
--     runSolverOnSMT
--   vtest r $ Map.fromList [ ("result_0", 4)
--                          , ("result2_0", 8)
--                          ]
