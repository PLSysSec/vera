module Lang where
import           BenchUtils
import qualified Data.Map         as Map
import           DSL.Typed        (Type (..))
import           Generate.CodeGen
import           Generate.Lang
import           Generate.State
import           Utils

langTests :: BenchTest
langTests = benchTestGroup "Lang" [ declTest ]

declTest :: BenchTest
declTest = benchTestCase "decl" $ do

  r <- evalCodegen Nothing $ do

    let decls = [ declare Signed "x"
                , declare Signed "y"
                , (v "x") `assign` (number Signed 5)
                , (v "y") `assign` (number Signed 10)
                ]
    genBodySMT decls
    runSolverOnSMT
  vtest r $ Map.fromList [ ("x_1", 5)
                         , ("y_1", 10)
                         ]
