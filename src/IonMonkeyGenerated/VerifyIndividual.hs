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

             , version_ $ v "rvf"
             , assert_ $ not_ (v "rvf" .->. "isEmpty")

             , assert_ $ (v "rvf" .->. "lower") .=>. jsIntMin
             , assert_ $ (v "rvf" .->. "lower") .<=. jsIntMax
             , assert_ $ (v "rvf" .->. "upper") .=>. jsIntMin
             , assert_ $ (v "rvf" .->. "upper") .<=. jsIntMax
            , assert_ $ (v "rvf" .->. "upper") .=>. (v "rvf" .->. "lower")

             , implies_ (not_ $ v "rvf" .->. "hasInt32LowerBound") (v "rvf" .->. "lower" .==. jsIntMin)
             , implies_ (not_ $ v "rvf" .->. "hasInt32UpperBound") (v "rvf" .->. "upper" .==. jsIntMax)

             , implies_ (v "rvf" .->. "canBeNegativeZero") (call "contains" [v "rvf", n Signed 0])
             , assert_ $ (v "rvf" .->. "maxExponent" .==. includesInfinityAndNan) .||. (v "rvf" .->. "maxExponent" .==. includesInfinity) .||. (v "rvf" .->. "maxExponent" .<=. maxFiniteExponent)
             , implies_ (v "rvf" .->. "hasInt32LowerBound" .&&. (v "rvf" .->. "hasInt32UpperBound")) (v "rvf" .->. "maxExponent" .==. (fpExp (cast (max_ (abs_ $ v "rvf" .->. "lower") (abs_ $ v "rvf" .->. "upper")) Double)))

             , implies_ (v "rvf" .->. "hasInt32LowerBound") (v "rvf" .->. "maxExponent" .=>. (fpExp (cast (abs_ $ v "rvf" .->. "lower") Double)))

             , implies_ (v "rvf" .->. "hasInt32UpperBound") (v "rvf" .->. "maxExponent" .=>. (fpExp (cast (abs_ $ v "rvf" .->. "upper") Double)))
             , return_ $ v "rvf"
             ]
  in Function "wellFormedRange" (c "range") [] body

fInRange :: FunctionDef
fInRange =
  let args = [ ("fval", t Double)
             , ("frange", c "range")
             ]
      body = [ declare (t Bool) "infHolds"
             , v "infHolds" `assign` (testImplies (isInf $ v "fval") ((v "frange" .->. "maxExponent") .=>. includesInfinity))

             , declare (t Bool) "nanHolds"
             , v "nanHolds" `assign` (testImplies (isNan $ v "fval") ((v "frange" .->. "maxExponent") .==. includesInfinityAndNan))

             , declare (t Bool) "negzHolds"
             , v "negzHolds" `assign` (testImplies (isNegZero $ v "fval") (v "frange" .->. "canBeNegativeZero"))

             , declare (t Bool) "fractHolds"
             , v "fractHolds" `assign` (testImplies ((not_ $ isNan $ v "fval") .&&. (not_ $ v "fval" .==. (jsCeil $ v "fval"))) (v "frange" .->. "canHaveFractionalPart"))

             , declare (t Bool) "hasLowHolds"
             , v "hasLowHolds" `assign` (testImplies ((v "fval" .<. (cast (v "frange" .->. "lower") Double)) .&&. (not_ $ isNan $ v "fval")) (not_ $ v "frange" .->. "hasInt32LowerBound"))

             , declare (t Bool) "hasHighHolds"
             , v "hasHighHolds" `assign` (testImplies ((v "fval" .>. (cast (v "frange" .->. "upper") Double)) .&&. (not_ $ isNan $ v "fval")) (not_ $ v "frange" .->. "hasInt32UpperBound"))

             -- try to help out the solver
             , assert_ $ (isInf $ v "fval") .||. (isNan $ v "fval") .||. (fpExp (v "fval") .<=. maxFiniteExponent)
             , implies_ ((v "fval" .<=. d Double 1) .&&. (v "fval" .=>. d Double (-1))) (fpExp (v "fval") .==. n Unsigned16 0)

             , declare (t Bool) "underExp"
             , v "underExp" `assign` ((fpExp $ v "fval") .<=. (v "frange" .->. "maxExponent"))

             , return_ $ (not_ $ v "frange" .->. "isEmpty") .&&. v "infHolds" .&&. v "nanHolds" .&&. v "negzHolds" .&&. v "fractHolds" .&&. v "hasLowHolds" .&&. v "hasHighHolds" .&&. v "underExp"

             ]
  in Function "fInRange" (t Bool) args body

--
-- Automatic testing infrastructure
--

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
                  | Set { testName :: String
                        , setOp    :: FunctionDef
                        }

isBinary :: TestFunction -> Bool
isBinary Binary{} = True
isBinary _        = False

isConstant :: TestFunction -> Bool
isConstant Constant{} = True
isConstant _          = False

isSet :: TestFunction -> Bool
isSet Set{} = True
isSet _     = False


-- union and intersection verification conditions

testUnion :: TestFunction -> Codegen ()
testUnion fn = do
  setupAllFloat fn
  genBodySMT [vcall "verifyUnion" [v "left_range", v "right_range", v "result_range"]]

testIntersection :: TestFunction -> Codegen ()
testIntersection fn = do
  liftIO $ print "HERE"
  setupAllFloat fn
  -- genBodySMT [vcall "verifyIntersection" [v "left_range", v "right_range", v "result_range"]]

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

testFlUpper :: TestFunction -> Codegen ()
testFlUpper fn = do
  setupAllFloat fn
  genBodySMT [vcall "verifyFlUpper" [v "result_range", v "result"]]

testFlLower :: TestFunction -> Codegen ()
testFlLower fn = do
  setupAllFloat fn
  genBodySMT [vcall "verifyFlLower" [v "result_range", v "result"]]

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
  genBodySMT [ vcall "verifyExp" [v "result_range", v "result"] ]

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
  else if isSet fn
       then setupSetOp (setOp fn) (testName fn)
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
              , (v "start_range")  `assign` (call "wellFormedRange" [])
              , (v "result_range") `assign` call fnName [v "start_range"]
              , assert_ $ call "fInRange" [v "start", v "start_range"]
              , v "start" `assign` v "start"
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
              , (v "left_range")   `assign` (call "wellFormedRange" [])
              , (v "right_range")  `assign` (call "wellFormedRange" [])
              , (v "result_range") `assign` call fnName [v "left_range", v "right_range"]
                -- Actually perform the JS operation
              , assert_ $ call "fInRange" [v "left", v "left_range"]
              , assert_ $ call "fInRange" [v "right", v "right_range"]
              , v "left" `assign` v "left"
              , v "right" `assign` v "right"
              , v "result" `assign` (v "left" `fn` v "right")
              , expect_ isSat (error "Has to start out SAT")
              ]
  genBodySMT verif

setupSetOp :: FunctionDef
           -> String
           -> Codegen ()
setupSetOp op fnName = do
  defineAll op
  let combo = if fnName == "union" then (.||.) else (.&&.)
      verif = [ declare (c "range") "left_rangey"
              , declare (c "range") "right_rangey"
              , (v "left_rangey")   `assign` (call "wellFormedRange" [])
              , (v "right_rangey")  `assign` (call "wellFormedRange" [])

              , declare (c "range") "result_rangey"
              , (v "result_rangey") `assign` call "intersect" [v "left_rangey", v "right_rangey"]

              , declare (t Double) "elem"
              , declare (t Bool) "isInRight"
              , declare (t Bool) "isInLeft"
              , declare (t Bool) "isInResult"
              , v "isInRight" `assign` (call "fInRange" [v "elem", v "right_rangey"])
              , v "isInLeft" `assign` (call "fInRange" [v "elem", v "left_rangey"])
              , v "isInResult" `assign` (call "fInRange" [v "elem", v "result_rangey"])
              , expect_ isSat (error "Should be sat")

              , assert_ $ (v "isInLeft" .&&. v "isInRight") .&&. (not_ $ v "isInResult")
              , expect_ isUnsat (\r -> showInt32Result "Failed intersect" r )
              ]
  genBodySMT verif

defineAll op = do
  class_ range
  define op
  define floatInRange
  define wellFormedRange
  define fInRange
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
  define nullRange
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
  define verifyUpperFl
  define verifyLowerFl
  define verifyUnion
  define verifyIntersection

