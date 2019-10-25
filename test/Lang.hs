module Lang where
import           BenchUtils
import qualified Data.Map         as Map
import           DSL.Typed        (Type (..))
import           Generate.CodeGen
import           Generate.Lang
import           Generate.State
import           Utils

langTests :: BenchTest
langTests = benchTestGroup "Lang" [ declTest
                                  , ifTest
                                  ]

declTest :: BenchTest
declTest = benchTestCase "decl" $ do

  r <- evalCodegen Nothing $ do

    let decls = [ declare Signed "x"
                , declare Signed "y"
                , (v "x") `assign` (number Signed 5)
                , (v "x") `assign` (number Signed 7)
                , (v "y") `assign` (number Signed 10)
                , (v "x") `assign` (number Signed 9)
                ]
    genBodySMT decls
    runSolverOnSMT
  vtest r $ Map.fromList [ ("x_0", 5)
                         , ("x_1", 7)
                         , ("x_2", 9)
                         , ("y_0", 10)
                         ]

ifTest :: BenchTest
ifTest = benchTestCase "if" $ do

  r <- evalCodegen Nothing $ do

    let decls = [ declare Signed "x"
                , declare Signed "y"
                , declare Signed "z"
                , (v "x") `assign` (number Signed 5)
                , (v "y") `assign` (number Signed 10)
                , (v "z") `assign` (number Signed 20)
                , if_ ((v "y") .<. (number Signed 11)) [ v "x" `assign` (number Signed 8)
                                                       , v "x" `assign` (number Signed 20)
                                                       ] (Just [v "x" `assign` (number Signed 12)])
                , if_ ((v "x") .<. (number Signed 4)) [ v "z" `assign` (number Signed 0) ] Nothing
                ]
    genBodySMT decls
    runSolverOnSMT
  vtest r $ Map.fromList [ ("x_0", 5)
                         , ("y_0", 10)
                         , ("x_1", 8)
                         , ("x_2", 20)
                         , ("x_3", 20)
                         , ("z_0", 20)
                         , ("z_1", 20)
                         ]



