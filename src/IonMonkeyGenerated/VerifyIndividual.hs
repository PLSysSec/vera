module IonMonkeyGenerated.VerifyIndividual where
import           Control.Monad                 (forM_)
import           Control.Monad.State.Strict    (liftIO)
import           Data.List                     (isInfixOf)
import qualified Data.Map                      as M
import           Data.Maybe                    (catMaybes)
import           DSL.DSL                       (isSat, isUnsat)
import           DSL.Typed                     (SMTResult (..), Type (..))
import           Generate.Lang
import           Generate.SMTAST
import           Generate.SMTGen
import           Generate.State
import           GHC.Float
import           IonMonkeyGenerated.Helpers
import           IonMonkeyGenerated.Objects
import           IonMonkeyGenerated.Operations
import           IonMonkeyGenerated.Verify
import           Prelude

wellFormedRange :: FunctionDef
wellFormedRange =
  let body = [ declare (c "range") "rvf"
             , assert_ $ (v "rvf" .->. "lower") .=>. jsIntMin
             , assert_ $ (v "rvf" .->. "lower") .<=. jsIntMax
             , assert_ $ (v "rvf" .->. "upper") .=>. jsIntMin
             , assert_ $ (v "rvf" .->. "upper") .<=. jsIntMax
             , implies_ (not_ $ v "rvf" .->. "hasInt32LowerBound") (v "rvf" .->. "lower" .==. jsIntMax)
             , implies_ (not_ $ v "rvf" .->. "hasInt32UpperBound") (v "rvf" .->. "upper" .==. jsIntMax)

             , implies_ (v "rvf" .->. "canBeNegativeZero") (call "contains" [v "rvf", n Signed 0])
             , assert_ $ (v "rvf" .->. "maxExponent" .==. includesInfinityAndNan) .||. (v "rvf" .->. "maxExponent" .==. includesInfinity) .||. (v "rvf" .->. "maxExponent" .<=. maxFiniteExponent)
             , implies_ (v "rvf" .->. "hasInt32LowerBound" .&&. (v "rvf" .->. "hasInt32UpperBound")) (v "rvf" .->. "maxExponent" .==. (fpExp (cast (max_ (abs_ $ v "rvf" .->. "lower") (abs_ $ v "rvf" .->. "upper")) Double)))
             , implies_ (v "rvf" .->. "hasInt32LowerBound") (v "rvf" .->. "maxExponent" .=>. (fpExp (cast (abs_ $ v "rvf" .->. "lower") Double)))
             , implies_ (v "rvf" .->. "hasInt32UpperBound") (v "rvf" .->. "maxExponent" .=>. (fpExp (cast (abs_ $ v "rvf" .->. "upper") Double)))
             ]
  in Function "newInt32InputRange" (c "range") [] body

data TestFunction = Binary { testName :: String
                           , binaryCppOp :: FunctionDef
                           , binaryJSOp :: (Codegen SExpr -> Codegen SExpr -> Codegen SExpr)
                           }
                  | Constant { testName :: String
                             , constCppOp :: FunctionDef
                             , constJSOp :: (Codegen SExpr -> Codegen SExpr -> Codegen SExpr)
                             }
                  | Unary { testName   :: String
                          , unaryCppOp :: FunctionDef
                          , unaryJSOp  :: (Codegen SExpr -> Codegen SExpr)
                          }

isBinary :: TestFunction -> Bool
isBinary Binary{} = True
isBinary _        = False

isConstant :: TestFunction -> Bool
isConstant Constant{} = True
isConstant _          = False


-- Int32 verification conditions

testLower :: TestFunction -> Codegen ()
testLower fn = do
  setupAlli32 fn
  genBodySMT [vcall "verifyLower" [v "result_range", v "result"]]

testUpper :: TestFunction -> Codegen ()
testUpper fn = do
  setupAlli32 fn
  genBodySMT [vcall "verifyUpper" [v "result_range", v "result"]]

testUB :: TestFunction -> Codegen ()
testUB fn = do
  setupAlli32 fn
  genBodySMT [vcall "verifyUB" [v "result_range", v "result"]]

-- Float verification conditions

testLowInvariant :: TestFunction -> Codegen ()
testLowInvariant fn = do
  setupAllFloat fn
  genBodySMT [vcall "verifyLowBoundInvariant" [v "result_range", v "result"]]

testHighInvariant :: TestFunction -> Codegen ()
testHighInvariant fn = do
  setupAllFloat fn
  genBodySMT [vcall "verifyUpBoundInvariant" [v "result_range", v "result"]]

testNegZ :: TestFunction -> Codegen ()
testNegZ fn = do
  setupAllFloat fn
  genBodySMT [vcall "verifyNegZ" [v "result_range", v "result"]]

testNan :: TestFunction -> Codegen ()
testNan fn = do
  setupAllFloat fn
  genBodySMT [vcall "verifyNan" [v "result_range", v "result"]]

testInf :: TestFunction -> Codegen ()
testInf fn = do
  setupAllFloat fn
  genBodySMT [vcall "verifyInf" [v "result_range", v "result"]]

testFract :: TestFunction -> Codegen ()
testFract fn = do
  setupAllFloat fn
  genBodySMT [vcall "verifyFract" [v "result_range", v "result"]]

testExp :: TestFunction -> Codegen ()
testExp fn = do
  setupAllFloat fn
  genBodySMT [vcall "verifyExp" [v "result_range", v "result"]]

-- General setup functions

setupAlli32 :: TestFunction -> Codegen ()
setupAlli32 fn = do
  if isBinary fn
  then setupi32 (binaryCppOp fn) (testName fn) (binaryJSOp fn)
  else if isConstant fn
       then setupConstanti32 (constCppOp fn) (testName fn) (constJSOp fn)
       else setupUnaryi32 (unaryCppOp fn) (testName fn) (unaryJSOp fn)

setupAllFloat :: TestFunction -> Codegen ()
setupAllFloat fn = do
  if isBinary fn
  then setupFloat (binaryCppOp fn) (testName fn) (binaryJSOp fn)
  else setupUnaryFloat (unaryCppOp fn) (testName fn) (unaryJSOp fn)

-- Individual setup functions

setupUnaryi32 :: FunctionDef
              -> String
              -> (Codegen SExpr -> Codegen SExpr)
              -> Codegen ()
setupUnaryi32 op fnName fn = do
  defineAll op
  let verif = [ declare (c "range") "start_range"
              , declare (t Signed) "start"
              , declare (c "range") "result_range"
              , declare (t Signed) "result"
              , (v "start_range")   `assign` (call "newInt32InputRange" [])
              , (v "result_range") `assign` call fnName [v "start_range"]
                -- Actually perform the JS operation
              , (v "start")  `assign` (call "intInRange" [v "start_range"])
              , (v "result") `assign` (fn $ v "start")
              , expect_ isSat (error "Has to start out SAT")
              ]
  -- Once we generate the SMT, we can verify each condition
  genBodySMT verif

setupUnaryFloat :: FunctionDef
                -> String
                -> (Codegen SExpr -> Codegen SExpr)
                -> Codegen ()
setupUnaryFloat op fnName fn = do
  defineAll op
  let verif = [ declare (c "range") "start_range"
              , declare (t Double) "start"
              , declare (c "range") "result_range"
              , declare (t Double) "result"
              , (v "start_range")  `assign` (call "newFloatInputRange" [])
              , (v "result_range") `assign` call fnName [v "start_range"]
              , (v "start")  `assign` (call "floatInRange" [v "start_range"])
              , (v "result") `assign` (fn $ v "start")
              , expect_ isSat (error "Has to start out SAT")
              ]
  genBodySMT verif

setupConstanti32 :: FunctionDef
                 -> String
                 -> (Codegen SExpr -> Codegen SExpr -> Codegen SExpr)
                 -> Codegen ()
setupConstanti32 op fnName fn = do
  defineAll op
  let verif = [ declare (c "range") "left_range"
              , declare (t Signed) "left"
              , declare (t Signed) "right"
              , declare (c "range") "result_range"
              , declare (t Signed) "result"
              , (v "left_range")   `assign` (call "newInt32InputRange" [])
              , (v "result_range") `assign` call fnName [v "left_range", v "right"]
                -- Actually perform the JS operation
              , (v "left")  `assign` (call "intInRange" [v "left_range"])
              , (v "result") `assign` (v "left" `fn` v "right")
              , expect_ isSat (error "Has to start out SAT")
              ]
  genBodySMT verif

setupi32 :: FunctionDef
         -> String
         -> (Codegen SExpr -> Codegen SExpr -> Codegen SExpr)
         -> Codegen ()
setupi32 op fnName fn = do
  defineAll op
  let verif = [ declare (c "range") "left_range"
              , declare (t Signed) "left"
              , declare (c "range") "right_range"
              , declare (t Signed) "right"
              , declare (c "range") "result_range"
              , declare (t Signed) "result"
              , (v "left_range")   `assign` (call "newInt32InputRange" [])
              , (v "right_range")  `assign` (call "newInt32InputRange" [])
              , (v "result_range") `assign` call fnName [v "left_range", v "right_range"]
                -- Actually perform the JS operation
              , (v "left")  `assign` (call "intInRange" [v "left_range"])
              , (v "right") `assign` (call "intInRange" [v "right_range"])
              , (v "result") `assign` (v "left" `fn` v "right")
              , expect_ isSat (error "Has to start out SAT")
              ]
  genBodySMT verif

setupFloat :: FunctionDef
           -> String
           -> (Codegen SExpr -> Codegen SExpr -> Codegen SExpr)
           -> Codegen ()
setupFloat op fnName fn = do
  defineAll op
  let verif = [ declare (c "range") "left_range"
              , declare (t Double) "left"
              , declare (c "range") "right_range"
              , declare (t Double) "right"
              , declare (c "range") "result_range"
              , declare (t Double) "result"
              , (v "left_range")   `assign` (call "newFloatInputRange" [])
              , (v "right_range")  `assign` (call "newFloatInputRange" [])
              , (v "result_range") `assign` call fnName [v "left_range", v "right_range"]
                -- Actually perform the JS operation
              , (v "left")  `assign` (call "floatInRange" [v "left_range"])
              , (v "right") `assign` (call "floatInRange" [v "right_range"])
              , (v "result") `assign` (v "left" `fn` v "right")
              , expect_ isSat (error "Has to start out SAT")
              ]
  genBodySMT verif

defineAll op = do
  class_ range
  define op
  define floatInRange
  define newFloatInputRange
  define verifySaneRange
  define verifyNegZero
  define verifyNan
  define verifyInf
  define verifyExp
  define canBeInfiniteOrNan
  define setLowerInit
  define optimize
  define setUpperInit
  define range3
  define range6
  define range4
  define canBeFiniteNonNegative
  define exponentImpliedByInt32Bounds
  define numBits
  define canBeNan
  define verifyFpBound
  define canBeZero
  define newInt32InputRange
  define isFiniteNonNegative
  define isFiniteNegative
  define newUInt32Range
  define intInRange
  define verifyLower
  define verifyUpper
  define verifyUB
  define contains
  define hasInt32Bounds
  define missingAnyInt32Bounds
  define verifyLowBoundInvariant
  define verifyUpBoundInvariant
  define newInt32Range
  define countLeadingZeroes
  define countOnes
  define canHaveSignBitSet
  define verifyFract

