module JavaScript ( jsTests100
                  , jsTests1000
                  , jsBinOpTest) where
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


-- | Wrapper for runnign QC more than 100 times
-- nr is the number of tests to run
benchTestPropertyQ name prop nr = benchTestProperty name $ Q.withMaxSuccess nr prop

jsTests100 :: BenchTest
jsTests100 = benchTestGroup "JS100" [
    benchTestGroup "Arithmetic binary ops" [ jsBinOpTest JSAdd 100
                                           , jsBinOpTest JSSub 100
                                           , jsBinOpTest JSMul 100
                                           , jsBinOpTest JSMin 100
                                           , jsBinOpTest JSMax 100 ]
 ,  benchTestGroup "Bitwise ops" [ jsBitI32Test JSAnd 100
                                 , jsBitI32Test JSOr 100
                                 , jsBitI32Test JSXor 100
                                 , jsBitI32Test JSShl 100
                                 , jsShrTest 100
                                 , jsUshrTest 100
                                 , jsNotTest 100 ]
  , benchTestGroup "Unary ops" [ jsUniOpTest JSAbs 100
                               , jsUniOpTest JSFloor 100
                               , jsUniOpTest JSCeil 100
                               , jsUniOpTest JSSign 100 ]
  ]

jsTests1000 :: BenchTest
jsTests1000 = benchTestGroup "JS1000" [
    benchTestGroup "Arithmetic binary ops" [ jsBinOpTest JSAdd 1000
                                           , jsBinOpTest JSSub 1000
                                           , jsBinOpTest JSMul 1000
                                           , jsBinOpTest JSMin 1000
                                           , jsBinOpTest JSMax 1000 ]
 ,  benchTestGroup "Bitwise ops" [ jsBitI32Test JSAnd 1000
                                 , jsBitI32Test JSOr 1000
                                 , jsBitI32Test JSXor 1000
                                 , jsBitI32Test JSShl 1000
                                 , jsShrTest 1000
                                 , jsUshrTest 1000
                                 , jsNotTest 1000 ]
  , benchTestGroup "Unary ops" [ jsUniOpTest JSAbs 1000
                               , jsUniOpTest JSFloor 1000
                               , jsUniOpTest JSCeil 1000
                               , jsUniOpTest JSSign 1000 ]
  ]

jsBitI32Test bop nr = benchTestPropertyQ ("QuickCheck " ++ show bop) jsT nr
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

jsShrTest nr = benchTestPropertyQ ("QuickCheck " ++ show bop) jsT nr
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

jsUshrTest nr = benchTestPropertyQ ("QuickCheck " ++ show bop) jsT nr
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

jsNotTest nr = benchTestPropertyQ ("QuickCheck " ++ show uop) jsT nr
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


jsUniOpTest uop nr = benchTestPropertyQ ("QuickCheck " ++ show uop) jsT nr
  where jsT :: Double -> Q.Property
        jsT x = Q.monadicIO $ do
          jsRes <- Q.run $ js uop x
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.fpnum x
              T.named "result" $ (jsUniOpToFunc uop) xv
              T.runSolver
          let (Just smtRes) = M.lookup "result" vars
          Q.assert $ smtRes == jsRes

jsBinOpTest bop nr = benchTestPropertyQ ("QuickCheck " ++ show bop) jsT nr
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
