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
                                  , classTest
                                  , ifTest
                                  , callTest
                                  , returnTest
                                  , classArgTest
                                  , classMethodTest
                                  , returnClassTest
                                  ]

declTest :: BenchTest
declTest = benchTestCase "decl" $ do

  r <- evalCodegen Nothing $ do

    let decls = [ declare (t Signed) "x"
                , declare (t Signed) "y"
                , (v "x") `assign` (n Signed 5)
                , (v "x") `assign` (n Signed 7)
                , (v "y") `assign` (n Signed 10)
                , (v "x") `assign` (n Signed 9)
                ]
    genBodySMT decls
    runSolverOnSMT
  vtest r $ Map.fromList [ ("x_1", 5)
                         , ("x_2", 7)
                         , ("x_3", 9)
                         , ("y_1", 10)
                         ]

classTest :: BenchTest
classTest = benchTestCase "class" $ do
  r <- evalCodegen Nothing $ do

    class_ $ ClassDef "myclass" [ ("myint", Signed)
                                , ("ruined", Signed)
                                , ("mybool", Bool)
                                ] []
    let decls = [ declare (c "myclass") "x"
                , declare (t Signed) "num"
                , ((v "x") .->. "myint")  `assign` (n Signed 5)
                , ((v "x") .->. "ruined") `assign` (n Signed 6)
                , ((v "x") .->. "myint")  `assign` (n Signed 7)
                ]
    genBodySMT decls
    runSolverOnSMT

  vtest r $ Map.fromList [ ("x_myint_1", 5)
                         , ("x_ruined_1", 6)
                         , ("x_myint_2", 7)
                         ]


ifTest :: BenchTest
ifTest = benchTestCase "if" $ do

  r <- evalCodegen Nothing $ do

    let decls = [ declare (t Signed) "x"
                , declare (t Signed) "y"
                , declare (t Signed) "z"
                , (v "x") `assign` (n Signed 5)
                , (v "y") `assign` (n Signed 10)
                , (v "z") `assign` (n Signed 20)
                , if_ ((v "y") .<. (n Signed 11)) [ v "x" `assign` (n Signed 8)
                                                  , v "x" `assign` (n Signed 20)
                                                  ] [v "x" `assign` (n Signed 12)]
                , if_ ((v "x") .<. (n Signed 4)) [ v "z" `assign` (n Signed 0) ] []
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
               , (v "ret") `assign` (v "x" .+. v "y")
               , return_ (v "ret")
               ]
    define $ Function "fun" (t Signed) args body
    genBodySMT [ declare (t Signed) "result"
               , (v "result") `assign` call "fun" [n Signed 5, n Signed 10]
               ]
    genBodySMT [ declare (t Signed) "result2"
               , (v "result2") `assign` call "fun" [n Signed 2, n Signed 4]
               ]
    runSolverOnSMT
  vtest r $ Map.fromList [ ("result_1", 15)
                         , ("result2_1", 6)
                         ]

returnTest :: BenchTest
returnTest = benchTestCase "return" $ do

  r <- evalCodegen Nothing $ do

    let args = [("x", t Signed)]
        body = [ if_ ((v "x") .<. (n Signed 4))
                 [ return_ (n Signed 4) ] [return_ (n Signed 8)]
               ]
    define $ Function "fun" (t Signed) args body

    genBodySMT [ declare (t Signed) "result"
               , (v "result") `assign` call "fun" [n Signed 3]
               ]
    genBodySMT [ declare (t Signed) "result2"
               , (v "result2") `assign` call "fun" [n Signed 5]
               ]
    runSolverOnSMT
  vtest r $ Map.fromList [ ("result_1", 4)
                         , ("result2_1", 8)
                         ]

classArgTest :: BenchTest
classArgTest = benchTestCase "class arg" $ do

  r <- evalCodegen Nothing $ do

    class_ $ ClassDef "myclass" [ ("myint", Signed) ] []

    let args = [("x", c "myclass")]
        body = [ if_ ((v "x" .->. "myint") .<. (n Signed 1))
                 [ return_ (n Signed 1) ] [return_ (n Signed 5)]
               ]
    define $ Function "fun" (t Signed) args body

    genBodySMT [ declare (t Signed) "result"
               , declare (c "myclass") "a1"
               , (v "a1") .->. "myint" `assign` (n Signed 0)
               , (v "result") `assign` call "fun" [v "a1"]
               ]
    genBodySMT [ declare (t Signed) "result2"
               , declare (c "myclass") "a2"
               , (v "a2") .->. "myint" `assign` (n Signed 5)
               , (v "result2") `assign` call "fun" [v "a2"]
               ]
    runSolverOnSMT
  vtest r $ Map.fromList [ ("result_1", 1)
                         , ("result2_1", 5)
                         ]

classMethodTest :: BenchTest
classMethodTest = benchTestCase "class method" $ do

  r <- evalCodegen Nothing $ do

    let body = [ if_ ((f "myint") .<. (n Signed 1))
               [ return_ (n Signed 1) ] [return_ (n Signed 5)]
               ]
        func = Function "fun" (t Signed) [] body

    class_ $ ClassDef "myclass" [ ("myint", Signed) ] [func]

    genBodySMT [ declare (t Signed) "result"
               , declare (c "myclass") "a1"
               , (v "a1") .->. "myint" `assign` (n Signed 0)
               , (v "result") `assign` (method (v "a1") "fun" [])
               ]

    genBodySMT [ declare (t Signed) "result2"
               , declare (c "myclass") "a2"
               , (v "a2") .->. "myint" `assign` (n Signed 1)
               , (v "result2") `assign` (method (v "a2") "fun" [])
               ]

    runSolverOnSMT
  vtest r $ Map.fromList [ ("result_1", 1)
                         , ("result2_1", 5)
                         ]

returnClassTest :: BenchTest
returnClassTest = benchTestCase "return class" $ do

  r <- evalCodegen Nothing $ do

    class_ $ ClassDef "myclass" [ ("myint", Signed)
                                , ("myotherint", Signed)
                                ] []

    let args = []
        body = [ declare (c "myclass") "result"
               , (v "result") .->. "myint" `assign` (n Signed 1)
               , (v "result") .->. "myotherint" `assign` (n Signed 2)
               , return_ (v "result")
               ]
    define $ Function "fun" (c "myclass") args body

    genBodySMT [ declare (c "myclass") "test"
               , (v "test") `assign` call "fun" []
               ]

    runSolverOnSMT
  vtest r $ Map.fromList [ ("test_myint_1", 1)
                         , ("test_myotherint_1", 2)
                         ]
