module JavaScript (jsTests) where
import           BenchUtils
import qualified Data.Map   as M
import           DSL.DSL    as D
import qualified DSL.Typed  as T
import           Prelude    hiding (and, max, min, not, pi)
import           Utils
import           ActiveCode.JavaScript
import qualified Test.QuickCheck.Monadic      as Q
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck        as Q

jsTests :: BenchTest
jsTests = benchTestGroup "JavaScript tests" [
    benchTestGroup "Arithmetic binary ops" [ jsBinOpTest JSAdd
                                           , jsBinOpTest JSSub
                                           , jsBinOpTest JSMul ]
  , benchTestGroup "Bitwise ops" [ jsBinOpTest JSAnd
                                 , jsBinOpTest JSOr
                                 , jsBinOpTest JSXor
                                 , jsBinOpTest JSShl
                                 , jsBinOpTest JSShr
                                 , jsBinOpTest JSUshr
                                 , jsUniOpTest JSNot ]
  , benchTestGroup "Unary ops" [ jsBinOpTest JSMin
                               , jsBinOpTest JSMax
                               , jsUniOpTest JSAbs
                               , jsUniOpTest JSFloor
                               , jsUniOpTest JSCeil
                               , jsUniOpTest JSSign  ]
  ]


jsUniOpTest uop = benchTestProperty ("QuickCheck " ++ show uop) jsT
  where jsT :: Double -> Q.Property
        jsT x = Q.monadicIO $ do
          jsRes <- Q.run $ js uop x
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.fpnum x
              T.named "result" $ (jsUniOpToFunc uop) xv
              T.runSolver
          let (Just smtRes) = M.lookup "result" vars
          Q.assert $ smtRes == jsRes

jsBinOpTest bop = benchTestProperty ("QuickCheck " ++ show bop) jsT
  where jsT :: Double -> Double -> Q.Property
        jsT x y = Q.monadicIO $ do
          jsRes <- Q.run $ js bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.fpnum x
              yv <- T.fpnum y
              T.named "result" $ (jsBinOpToFunc bop) xv yv
              T.runSolver
          let (Just smtRes) = M.lookup "result" vars
          Q.assert $ smtRes == jsRes

jsUniOpToFunc :: JSOp -> (T.VNode -> D.Verif T.VNode)
jsUniOpToFunc op = case op of
  JSNot   -> T.jsNot
  JSAbs   -> T.jsAbs
  JSFloor -> T.jsFloor
  JSCeil  -> T.jsCeil
  JSSign  -> T.jsSign

jsBinOpToFunc :: JSOp -> (T.VNode -> T.VNode -> D.Verif T.VNode)
jsBinOpToFunc op = case op of
  JSAdd  -> T.jsAdd
  JSSub  -> T.jsSub
  JSAnd  -> T.jsAnd
  JSOr   -> T.jsOr
  JSXor  -> T.jsXor
  JSMul  -> T.jsMul
  JSShl  -> T.jsShl
  JSShr  -> T.jsShr
  JSUshr -> T.jsUshr
  JSMin  -> T.jsMin
  JSMax  -> T.jsMax
