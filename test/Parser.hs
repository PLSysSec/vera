{-# LANGUAGE QuasiQuotes #-}
module Parser where
import           BenchUtils
import           Control.Monad.IO.Class 
import qualified Data.Map        as Map
import           DSL.Typed       (Type (..))
import           Generate.Lang as L
import           Generate.SMTAST
import           Generate.QQ
import           Generate.SMTGen
import           Generate.State
import           Generate.Parser (parseExpr, parseFunc, parseProgram)
import           Utils
import           Test.Tasty.HUnit
import           Text.RawString.QQ

parserTests :: BenchTest
parserTests = benchTestGroup "Parser" [ numTest, ternTest, eqTest, castTest, fieldTest, jsTest, negTest, mathTest, funTest, progTest]

numTest :: BenchTest
numTest = benchTestCase "num" $ do
  let p = unwrap $ parseExpr "(int32_t) 12 + (int32_t) 36.2"
  p `ceqs` Add (i32 12) (Cast (f64 36.2) Signed)

eqTest :: BenchTest
eqTest = benchTestCase "eq" $ do
  let p = unwrap $ parseExpr "(int32_t) 12 == (int32_t) 13"
  p `ceqs` Eq (i32 12) (i32 13)

ternTest :: BenchTest
ternTest = benchTestCase "ternary" $ do
  let p = unwrap $ parseExpr "(int32_t) 12 == (int32_t) 13 ? (int32_t) 4 : (int32_t) 5"
  p `ceqs` Tern (Eq (i32 12) (i32 13)) (i32 4) (i32 5)

castTest :: BenchTest
castTest = benchTestCase "cast" $ do
  let p = unwrap $ parseExpr "(bool) ((int32_t) 2 == (int32_t) 2)"
  p `ceqs` Cast (Eq (i32 2) (i32 2)) Bool
  let p = unwrap $ parseExpr "(bool) (int32_t) 2 == (int32_t) 2"
  p `ceqs` Eq (Cast (i32 2) Bool) (i32 2)

fieldTest :: BenchTest
fieldTest = benchTestCase "field" $ do
  let p = unwrap $ parseExpr "this.foo + (int32_t) 3"
  p `ceqs` Add (FieldExpr "foo") (i32 3)

jsTest :: BenchTest
jsTest = benchTestCase "js" $ do
  let p = unwrap $ parseExpr "js::sub(js::ceil(this.foo), this.bar)"
  p `ceqs` JSSub (JSCeil (FieldExpr "foo")) (FieldExpr "bar")

mathTest :: BenchTest
mathTest = benchTestCase "math" $ do
  let p = unwrap $ parseExpr "math::is_inf(this.foo)"
  p `ceqs` IsInf (FieldExpr "foo")

negTest :: BenchTest
negTest = benchTestCase "neg" $ do
  let p = unwrap $ parseExpr "math::is_neg(this.foo)"
  p `ceqs` IsNegative (FieldExpr "foo")

funTest :: BenchTest
funTest = benchTestCase "fun" $ do
  let p = unwrap $ parseFunc [r|uint8_t doThing(double f) {
      uint16_t y;
      y = (uint16_t) f;
      if (x == 0.0) {
        y += (uint16_t) 6;
      } else if (0.1)
        y = (uint16_t) 7;

      return (uint8_t) y;
    }
  |]
  L.fName p @=? "doThing"
  
progFileTest :: BenchTest
progFileTest = benchTestCase "progFile" $ do
  let Program fs cs = [progFile|test/test.lejit|]
  length fs @=? 0
  length cs @=? 1

progTest :: BenchTest
progTest = benchTestCase "prog" $ do
  let Program fs cs = [prog|
    class Foo {
      uint8_t bar(uint8_t x) {

      }
    } 
  |]
  length fs @=? 0
  length cs @=? 1

ceq :: Codegen SExpr -> Codegen SExpr -> IO ()
ceq c1 c2 = evalCodegen Nothing $ do
  l <- c1
  r <- c2
  liftIO $ l @=? r

ceqs :: Codegen SExpr -> SExpr -> IO ()
ceqs c1 s = evalCodegen Nothing $ do
  l <- c1
  liftIO $ l @=? s

i32 :: Integer -> SExpr
i32 i = NumExpr $ SNum Signed i

f64 :: Double -> SExpr
f64 f = NumExpr $ FNum Double f

unwrap :: Show a => Either a b -> b
unwrap (Left err) = error $ show err
unwrap (Right x) = x