module JavaScript (jsTests, jsBinOpTest) where
import           ActiveCode.JavaScript
import           BenchUtils
import           Data.Int
import qualified Data.Map                as M
import           Data.Word
import           DSL.DSL                 as D
import qualified DSL.Typed               as T
import           Prelude                 hiding (and, max, min, not, pi)
import qualified Test.QuickCheck.Monadic as Q
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck   as Q
import           Utils

-- | QC tests to run
nrTests :: Int
nrTests = 100

-- | Wrapper for runnign QC more than 100 times
benchTestPropertyQ name prop = benchTestProperty name $ Q.withMaxSuccess nrTests prop

jsTests :: BenchTest
jsTests = benchTestGroup "JS" [
    benchTestGroup "Arithmetic binary ops" [ jsBinOpTest JSAdd
                                           , jsBinOpTest JSSub
                                           , jsBinOpTest JSMul
                                           , jsBinOpTest JSMin
                                           , jsBinOpTest JSMax ]
 ,  benchTestGroup "Bitwise ops" [ jsBitI32Test JSAnd
                                 , jsBitI32Test JSOr
                                 , jsBitI32Test JSXor
                                 , jsBitI32Test JSShl
                                 , jsShrTest
                                 , jsUshrTest
                                 , jsNotTest ]
  , benchTestGroup "Unary ops" [ jsUniOpTest JSAbs
                               , jsUniOpTest JSFloor
                               , jsUniOpTest JSCeil
                               , jsUniOpTest JSSign  ]
  ]


jsBitI32Test bop = benchTestPropertyQ ("QuickCheck " ++ show bop) jsT
  where jsT :: Int32 -> Word32 -> Q.Property
        jsT x y = Q.monadicIO $ do
          jsRes <- Q.run $ js bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.named "x" $ T.num $ toInteger x
              yv <- T.named "y" $ T.unum $ toInteger y
              smtRes <- T.named "smtRes" $ (jsBinOpToFunc bop) xv yv
              jsResV <- T.named "jsRes" $ T.num $ toInteger (jsRes :: Int32)
              ok <- D.iseq (T.vnode smtRes) (T.vnode jsResV)
              D.named "ok" ok
              T.runSolver
          let (Just ok) = M.lookup "ok" vars
          Q.assert $ ok == 1

jsShrTest = benchTestPropertyQ ("QuickCheck " ++ show bop) jsT
  where jsT :: Int32 -> Word32 -> Q.Property
        jsT x y = Q.monadicIO $ do
          jsRes <- Q.run $ js bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.named "x" $ T.num $ toInteger x
              yv <- T.named "y" $ T.unum $ toInteger y
              smtRes <- T.named "smtRes" $ (jsBinOpToFunc bop) xv yv
              jsResV <- T.named "jsRes" $ T.num $ toInteger (jsRes :: Int32)
              ok <- D.iseq (T.vnode smtRes) (T.vnode jsResV)
              D.named "ok" ok
              T.runSolver
          let (Just ok) = M.lookup "ok" vars
          Q.assert $ ok == 1
        bop = JSShr

jsUshrTest = benchTestPropertyQ ("QuickCheck " ++ show bop) jsT
  where jsT :: Word32 -> Word32 -> Q.Property
        jsT x y = Q.monadicIO $ do
          jsRes <- Q.run $ js bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.named "x" $ T.num $ toInteger x
              yv <- T.named "y" $ T.unum $ toInteger y
              smtRes <- T.named "smtRes" $ (jsBinOpToFunc bop) xv yv
              jsResV <- T.named "jsRes" $ T.num $ toInteger (jsRes :: Word32)
              ok <- D.iseq (T.vnode smtRes) (T.vnode jsResV)
              D.named "ok" ok
              T.runSolver
          let (Just ok) = M.lookup "ok" vars
          Q.assert $ ok == 1
        bop = JSUshr

jsNotTest = benchTestPropertyQ ("QuickCheck " ++ show uop) jsT
  where jsT :: Int32 -> Q.Property
        jsT x = Q.monadicIO $ do
          jsRes <- Q.run $ js uop x
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.named "x" $ T.num $ toInteger x
              smtRes <- T.named "smtRes" $ (jsUniOpToFunc uop) xv
              jsResV <- T.named "jsRes" $ T.num $ toInteger (jsRes :: Int32)
              ok <- D.iseq (T.vnode smtRes) (T.vnode jsResV)
              D.named "ok" ok
              T.runSolver
          let (Just ok) = M.lookup "ok" vars
          Q.assert $ ok == 1
        uop = JSNot


jsUniOpTest uop = benchTestPropertyQ ("QuickCheck " ++ show uop) jsT
  where jsT :: Double -> Q.Property
        jsT x = Q.monadicIO $ do
          jsRes <- Q.run $ js uop x
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.fpnum x
              T.named "result" $ (jsUniOpToFunc uop) xv
              T.runSolver
          let (Just smtRes) = M.lookup "result" vars
          Q.assert $ smtRes == jsRes

jsBinOpTest bop = benchTestPropertyQ ("QuickCheck " ++ show bop) jsT
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
