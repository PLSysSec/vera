module IonMonkeyGenerated.Verify where
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
import           Prelude

wellFormedRange :: FunctionDef
wellFormedRange =
  let body = [ declare (c "range") "rvf"

             , version_ $ v "rvf"
             , assert_ $ not_ (v "rvf" .->. "isEmpty")

             , assert_ $ (v "rvf" .->. "lower") .=>. jsIntMin -- done
             , assert_ $ (v "rvf" .->. "lower") .<=. jsIntMax -- done
             , assert_ $ (v "rvf" .->. "upper") .=>. jsIntMin -- done
             , assert_ $ (v "rvf" .->. "upper") .<=. jsIntMax -- done
             , assert_ $ (v "rvf" .->. "upper") .=>. (v "rvf" .->. "lower")

             , implies_ (not_ $ v "rvf" .->. "hasInt32LowerBound") (v "rvf" .->. "lower" .==. jsIntMin) -- done
             , implies_ (not_ $ v "rvf" .->. "hasInt32UpperBound") (v "rvf" .->. "upper" .==. jsIntMax) -- done

             , implies_ (v "rvf" .->. "canBeNegativeZero") (call "contains" [v "rvf", n Signed 0])
               -- cant do

             , assert_ $ (v "rvf" .->. "maxExponent" .==. includesInfinityAndNan) .||. (v "rvf" .->. "maxExponent" .==. includesInfinity) .||. (v "rvf" .->. "maxExponent" .<=. maxFiniteExponent)

             , implies_ (v "rvf" .->. "hasInt32LowerBound" .&&. (v "rvf" .->. "hasInt32UpperBound")) (v "rvf" .->. "maxExponent" .==. (fpExp (cast (max_ (abs_ $ v "rvf" .->. "lower") (abs_ $ v "rvf" .->. "upper")) Double))) -- cant do

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

               -- this is not asserting that the actual exponent bits of the
               -- number are under 1023
             , assert_ $ (isInf $ v "fval") .||. (isNan $ v "fval") .||. (fpExp (v "fval") .<=. maxFiniteExponent)
             , implies_ ((v "fval" .<=. d Double 1) .&&. (v "fval" .=>. d Double (-1))) (fpExp (v "fval") .==. n Unsigned16 0)

             , declare (t Bool) "underExp"
             , v "underExp" `assign` ((fpExp $ v "fval") .<=. (v "frange" .->. "maxExponent"))

             , return_ $ (not_ $ v "frange" .->. "isEmpty") .&&. v "infHolds" .&&. v "nanHolds" .&&. v "negzHolds" .&&. v "fractHolds" .&&. v "hasLowHolds" .&&. v "hasHighHolds" .&&. v "underExp"

             ]
  in Function "fInRange" (t Bool) args body

vInRange :: FunctionDef
vInRange =
  let args = [ ("vval", t Signed)
             , ("vrange", c "range")
             ]
      body = [
              declare (t Bool) "hasLowHolds"
             , v "hasLowHolds" `assign` (v "vval" .>. (v "vrange" .->. "lower"))

             , declare (t Bool) "hasHighHolds"
             , v "hasHighHolds" `assign` (v "vval" .<. (v "vrange" .->. "upper"))

             , return_ $ (not_ $ v "vrange" .->. "isEmpty") .&&. v "hasLowHolds" .&&. v "hasHighHolds"

             ]
  in Function "vInRange" (t Bool) args body

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
  genBodySMT [vcall "verifyUnion" [v "isInLeft", v "isInRight", v "isInResult"]]

verifyUnion :: FunctionDef
verifyUnion =
  let args = [ ("in_left_union", t Bool)
             , ("in_right_union", t Bool)
             , ("in_result_union", t Bool)
             ]
      body = [ push_
             , assert_ $ (v "in_left_union") .||. (v "in_right_union")
             , assert_ $ not_ $ v "in_result_union"
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify union" r
             , pop_
             ]
  in Function "verifyUnion" Void args body

testIntersection :: TestFunction -> Codegen ()
testIntersection fn = do
  setupAllFloat fn
  genBodySMT [vcall "verifyIntersection" [v "isInLeft", v "isInRight", v "isInResult"]]

verifyIntersection :: FunctionDef
verifyIntersection =
  let args = [ ("in_left_inter", t Bool)
             , ("in_right_inter", t Bool)
             , ("in_result_inter", t Bool)
             ]
      body = [ push_
             , assert_ $ (v "in_left_inter") .&&. (v "in_right_inter")
             , assert_ $ not_ $ v "in_result_inter"
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify intersection" r
             , pop_
             ]
  in Function "verifyIntersection" Void args body

-- Int32 verification conditions

testLower :: TestFunction -> Codegen ()
testLower fn = do
  setupAlli32 fn
  genBodySMT [vcall "verifyLower" [v "result_range", v "result"]]

verifyLower :: FunctionDef
verifyLower =
  let args = [ ("result_range_l", c "range")
             , ("result_l", t Signed)
             ]
      body = [ push_
             , assert_ $ (v "result_range_l") .->. "hasInt32LowerBound"
             , assert_ $ (v "result_range_l") .->. "hasInt32UpperBound"
             , assert_ $ ((v "result_range_l") .->. "lower") .>. (v "result_l")
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify lower" r
             , pop_
             ]
  in Function "verifyLower" Void args body

testUpper :: TestFunction -> Codegen ()
testUpper fn = do
  setupAlli32 fn
  genBodySMT [vcall "verifyUpper" [v "result_range", v "result"]]

verifyUpper :: FunctionDef
verifyUpper =
  let args = [ ("result_range_u", c "range")
             , ("result_u", t Signed)
             ]
      body = [ push_
             , assert_ $ (v "result_range_u") .->. "hasInt32UpperBound"
             , assert_ $ (v "result_range_u") .->. "hasInt32LowerBound"
             , assert_ $ ((v "result_range_u") .->. "upper") .<. (v "result_u")
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify upper" r
             , pop_
             ]
  in Function "verifyUpper" Void args body

testUB :: TestFunction -> Codegen ()
testUB fn = do
  setupAlli32 fn
  genBodySMT [vcall "verifyUB" [v "result_range", v "result"]]

verifyUB :: FunctionDef
verifyUB =
  let args = [ ("result_range_undef", c "range")
             , ("result_undef", t Signed)
             ]
      body = [ push_
             , assert_ $ undef $ v "result_range_undef" .->. "lower"
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify lower UB" r
             , pop_
             , push_
             , assert_ $ undef $ v "result_range_undef" .->. "upper"
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify upper UB" r
             , pop_
             ]
  in Function "verifyUB" Void args body

-- Float verification conditions

testFlUpper :: TestFunction -> Codegen ()
testFlUpper fn = do
  setupAllFloat fn
  genBodySMT [vcall "verifyFlUpper" [v "result_range", v "result"]]

verifyUpperFl :: FunctionDef
verifyUpperFl =
  let args = [ ("result_range_ufl", c "range")
             , ("result_ufl", t Double)
             ]
      body = [ push_
             , assert_ $ (v "result_range_ufl") .->. "hasInt32UpperBound"
             , assert_ $ (cast ((v "result_range_ufl") .->. "upper") Double) .<. (v "result_ufl")
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify upper fl" r
             , pop_
             ]
  in Function "verifyFlUpper" Void args body

testFlLower :: TestFunction -> Codegen ()
testFlLower fn = do
  setupAllFloat fn
  genBodySMT [vcall "verifyFlLower" [v "result_range", v "result"]]

verifyLowerFl :: FunctionDef
verifyLowerFl =
  let args = [ ("result_range_fl", c "range")
             , ("result_fl", t Double)
             ]
      body = [ push_
             , assert_ $ (v "result_range_fl") .->. "hasInt32LowerBound"
             , assert_ $ (cast ((v "result_range_fl") .->. "lower") Double) .>. (v "result_fl")
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify fl lower" r
             , pop_
             ]
  in Function "verifyFlLower" Void args body

testBoundInvariants :: TestFunction -> Codegen ()
testBoundInvariants fn = do
  setupAllFloat fn
  genBodySMT [vcall "verifyBoundInvariants" [v "result_range", v "result"]]

verifyBoundInvariants :: FunctionDef
verifyBoundInvariants =
  let args = [ ("result_range_bi", c "range")
             , ("result_bi", t Double)
             ]
      body = [ push_
              -- It has no int32 lower bound...
             , assert_ $ not_ $ v "result_range_bi" .->. "hasInt32LowerBound"
               -- ...but the int32 lower bound isnt intmax
             , assert_ $ not_ $ (v "result_range_bi" .->. "lower") .==. jsIntMin
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify low bound invariant" r
             , pop_
             , push_
             , assert_ $ not_ $ v "result_range_bi" .->. "hasInt32UpperBound"
               -- ...but the int32 lower bound isnt intmax
             , assert_ $ not_ $ (v "result_range_bi" .->. "upper") .==. jsIntMax
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify high bound invariant" r
             , pop_
             ]
  in Function "verifyBoundInvariants" Void args body

testNegZ :: TestFunction -> Codegen ()
testNegZ fn = do
  setupAllFloat fn
  genBodySMT [vcall "verifyNegZ" [v "result_range", v "result"]]

verifyNegZero :: FunctionDef
verifyNegZero =
  let args = [ ("result_range_nz", c "range")
             , ("result_nz", t Double)
             ]
      body = [ push_
               -- It's negative zero...
             , assert_ $ (isNegZero $ v "result_nz")
               -- ... but the negative zero flag isn't set
             , assert_ $ not_ $ v "result_range_nz" .->. "canBeNegativeZero"
             , expect_ isUnsat $ \r -> showNegzResult "Failed to verify NZ" r
             , pop_
             ]
  in Function "verifyNegZ" Void args body

testNan :: TestFunction -> Codegen ()
testNan fn = do
  setupAllFloat fn
  genBodySMT [vcall "verifyNan" [v "result_range", v "result"]]

verifyNan :: FunctionDef
verifyNan =
  let args = [ ("result_range_nan", c "range")
             , ("result_nan", t Double)
             ]
      body = [ push_
               -- It's Nan
             , declare (t Bool) "isNan"
             , assert_ $ isNan $ v "result_nan"
             , v "isNan" `assign` (isNan $ v "result_nan")
               -- ... but the Nan exponent is not correct
             , assert_ $ not_ $ (v "result_range_nan" .->. "maxExponent") .==. includesInfinityAndNan
             , expect_ isUnsat $ \r -> showNanResult "Failed to verify Nan" r
             , pop_
             ]
  in Function "verifyNan" Void args body

testInf :: TestFunction -> Codegen ()
testInf fn = do
  setupAllFloat fn
  genBodySMT [vcall "verifyInf" [v "result_range", v "result"]]

verifyInf :: FunctionDef
verifyInf =
  let args = [ ("result_range_inf", c "range")
             , ("result_inf", t Double)
             ]
      body = [ push_
               -- It's inf
             , assert_ $ isInf $ v "result_inf"
               -- ... but the inf exponent is not correct
             , assert_ $ not_ $ (v "result_range_inf" .->. "maxExponent") .=>. includesInfinity
             , expect_ isUnsat $ \r -> showInfResult "Failed to verify Inf" r
             , pop_
             ]
  in Function "verifyInf" Void args body

testFract :: TestFunction -> Codegen ()
testFract fn = do
  setupAllFloat fn
  genBodySMT [vcall "verifyFract" [v "result_range", v "result"]]

verifyFract :: FunctionDef
verifyFract =
  let args = [ ("result_range_fract", c "range")
             , ("result_fract", t Double)
             ]
      body = [ push_
             , assert_ $ not_ $ isNan $ v "result_fract"
             , assert_ $ not_ $ v "result_fract" .==. (jsCeil $ v "result_fract")
             , assert_ $ not_ $ v "result_range_fract" .->. "canHaveFractionalPart"
             , expect_ isUnsat $ \r -> showFractResult "Failed to verify Fract" r
             , pop_
             ]
  in Function "verifyFract" Void args body

testExp :: TestFunction -> Codegen ()
testExp fn = do
  setupAllFloat fn
  genBodySMT [ vcall "verifyExp" [v "result_range", v "result"] ]

verifyExp :: FunctionDef
verifyExp =
  let args = [ ("result_range_exp", c "range")
             , ("result_exp", t Double)
             ]
      body = [ push_
             , assert_ $ not_ $ isNan $ v "result_exp"
             , assert_ $ not_ $ isInf $ v "result_exp"
             , assert_ $ (fpExp $ v "result_exp") .>. (v "result_range_exp" .->. "maxExponent")
             , expect_ isUnsat $ \r -> showExpResult "Failed to verify Exp" r
             , pop_
             ]
  in Function "verifyExp" Void args body

testBoundForm :: TestFunction -> Codegen ()
testBoundForm fn = do
  setupAllFloat fn
  genBodySMT [ vcall "verifyBoundForm" [v "result_range", v "result"] ]

verifyForm :: FunctionDef
verifyForm =
  let args = [ ("result_range_form", c "range")
             , ("result_form", t Double)
             ]
      body = [ push_
             , assert_ $ ((v "result_range_form" .->. "lower") .<. jsIntMin) .||. ((v "result_range_form" .->. "lower") .>. jsIntMax)
             , expect_ isUnsat $ \r -> showExpResult "Failed to verify bound form" r
             , pop_
             , push_
             , assert_ $ ((v "result_range_form" .->. "upper") .<. jsIntMin) .||. ((v "result_range_form" .->. "upper") .>. jsIntMax)
             , expect_ isUnsat $ \r -> showExpResult "Failed to verify bound form" r
             , pop_
             ]
  in Function "verifyBoundForm" Void args body

testExpForm :: TestFunction -> Codegen ()
testExpForm fn = do
  setupAllFloat fn
  genBodySMT [ vcall "verifyExpForm" [v "result_range", v "result"] ]

verifyExpForm :: FunctionDef
verifyExpForm =
  let args = [ ("result_range_bexp", c "range")
             , ("result_bexp", t Double)
             ]
      body = [ push_
             , assert_ $ v "result_range_bexp" .->. "maxExponent" .>. includesInfinity
             , assert_ $ v "result_range_bexp" .->. "maxExponent" .<. includesInfinityAndNan
             , expect_ isUnsat $ \r -> showExpResult "Failed to verify bound form" r
             , pop_
             ]
  in Function "verifyExpForm" Void args body

testExpBounds :: TestFunction -> Codegen ()
testExpBounds fn = do
  setupAllFloat fn
  genBodySMT [ vcall "verifyExpBounds" [v "result_range", v "result"] ]

verifyExpBounds :: FunctionDef
verifyExpBounds =
  let args = [ ("result_range_bexp2", c "range")
             , ("result_bexp2", t Double)
             ]
      body = [ push_
             , assert_ $ v "result_range_bexp2" .->. "hasInt32LowerBound"
             , assert_ $ (fpExp (cast (abs_ $ v "result_range_bexp2" .->. "lower") Double)) .>. (v "result_range_bexp2" .->. "maxExponent")
             , expect_ isUnsat $ \r -> showExpResult "Failed to verify bound form" r
             , pop_
             , push_
             , assert_ $ v "result_range_bexp2" .->. "hasInt32UpperBound"
             , assert_ $ (fpExp (cast (abs_ $ v "result_range_bexp2" .->. "upper") Double)) .>. (v "result_range_bexp2" .->. "maxExponent")
             , expect_ isUnsat $ \r -> showExpResult "Failed to verify bound form" r
             , pop_
             ]
  in Function "verifyExpBounds" Void args body




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
              , (v "start_range")   `assign` (call "wellFormedRange" [])
              , (v "result_range") `assign` call fnName [v "start_range"]
                -- Actually perform the JS operation
              , assert_ $ call "vInRange" [v "start", v "start_range"]
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
  let verif = [ declare (c "range") "start_range"
              , declare (t Signed) "start"
              , declare (t Signed) "right"
              , declare (c "range") "result_range"
              , declare (t Signed) "result"
              , (v "start_range")   `assign` (call "wellFormedRange" [])
              , (v "result_range") `assign` call fnName [v "start_range", v "right"]
                -- Actually perform the JS operation
              , assert_ $ call "vInRange" [v "start", v "start_range"]
              , (v "result") `assign` (v "start" `fn` v "right")
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
              , (v "left_range")   `assign` (call "wellFormedRange" [])
              , (v "right_range")  `assign` (call "wellFormedRange" [])
              , (v "result_range") `assign` call fnName [v "left_range", v "right_range"]
                -- Actually perform the JS operation
              , assert_ $ call "vInRange" [v "left", v "left_range"]
              , assert_ $ call "vInRange" [v "right", v "right_range"]
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
              , declare (t Double) "lefty"
              , (v "left_range")   `assign` (call "wellFormedRange" [])
              , (v "right_range")  `assign` (call "wellFormedRange" [])
              , (v "result_range") `assign` call fnName [v "left_range", v "right_range"]
                -- Actually perform the JS operation
              , assert_ $ call "fInRange" [v "left", v "left_range"]
              , assert_ $ call "fInRange" [v "right", v "right_range"]
              , v "left" `assign` v "left"
              , v "lefty" `assign` v "left"
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
  let verif = [ declare (c "range") "left_range"
              , declare (c "range") "right_range"
              , (v "left_range")   `assign` (call "wellFormedRange" [])
              , (v "right_range")  `assign` (call "wellFormedRange" [])

              , declare (c "range") "result_range"
              , (v "result_range") `assign` call fnName [v "left_range", v "right_range"]

              , declare (t Double) "elem"
              , declare (t Bool) "isInRight"
              , declare (t Bool) "isInLeft"
              , declare (t Bool) "isInResult"

              , v "isInRight" `assign` (call "fInRange" [v "elem", v "right_range"])
              , v "isInLeft" `assign` (call "fInRange" [v "elem", v "left_range"])
              , v "isInResult" `assign` (call "fInRange" [v "elem", v "result_range"])
              , expect_ isSat (error "Should be sat")
              ]
  genBodySMT verif

defineAll op = do
  class_ range
  define op
  define verifyIntersection
  define verifyExpForm
  define verifyUnion
  define verifyExpBounds
  define vInRange
  define wellFormedRange
  define fInRange
  define verifyForm
  define newFloatInputRange
  define canBeFiniteNegative
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
  define canBeZero
  define newInt32InputRange
  define isFiniteNonNegative
  define isFiniteNegative
  define newUInt32Range
  define verifyLower
  define verifyUpper
  define verifyUB
  define contains
  define hasInt32Bounds
  define missingAnyInt32Bounds
  define verifyBoundInvariants
  define newInt32Range
  define countLeadingZeroes
  define countOnes
  define canHaveSignBitSet
  define verifyFract
  define verifyUpperFl
  define verifyLowerFl

---
--- Printing
---

data VerifResult = Verified
                 | UnsatImpl
                 | OverlappingRange { counterexample :: M.Map String Double }
                 | BadLowerBound { counterexample :: M.Map String Double }
                 | BadUpperBound { counterexample :: M.Map String Double }
                 | UndefRange { counterexample :: M.Map String Double }
                 | NoNanFlag { counterexample :: M.Map String Double }
                 | NoInfFlag { counterexample :: M.Map String Double }
                 | NoNegzFlag { counterexample :: M.Map String Double }
                 deriving (Eq, Ord)

instance Show VerifResult where
    show (OverlappingRange ce) = "Upper and lower of result range may overlap\n:" ++
                                 prettyCounterexampleInts ce
    show (BadLowerBound ce)    = "Example operation can be outside of lower boud\n:" ++
                                 prettyCounterexampleInts ce
    show (BadUpperBound ce)    = "Example operation can be outside of upper bound\n" ++
                                 prettyCounterexampleInts ce
    show (UndefRange ce)       = "Example operation may introduce undefined behavior:\n" ++
                                 prettyCounterexampleInts ce
    show (NoNanFlag ce)        = "Example operation returns NAN without flag set:\n" ++
                                 (unlines $ getNanList ce)
    show (NoInfFlag ce)        = "Example operation returns INF without flag set:\n" ++
                                 (unlines $ getNanList ce)
    show (NoNegzFlag ce)       = "Example operation returns -0 without flag set:\n" ++
                                 (unlines $ getNegzList ce)
    show Verified              = "Verified!"
    show UnsatImpl             = "Verification failed (e.g., due to a timeout)"

showExpResult :: String -> SMTResult -> IO ()
showExpResult str result = error $ str ++ "\n" ++ (unlines $ getExpList $ example result)

getExpList :: M.Map String Double -> [String]
getExpList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "undef" `isInfixOf` str -> Nothing
                         _ | "start_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "left" `isInfixOf` str -> sstr str fl
                         _ | "start_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "start_range_canHaveFractionalPart" `isInfixOf` str -> sstr str fl
                         _ | "start_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "start_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "start_1" `isInfixOf` str -> sstr str fl
                         _ | "result_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "result_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "right_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "right_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "left_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "left_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "left_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "right_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "start_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "result_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "left_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "right_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "start_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "result_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "testy" `isInfixOf` str -> sstr str fl
                         _ | "right_1" `isInfixOf` str -> sstr str fl
                         _ | "left_1" `isInfixOf` str -> sstr str fl
                         _ | "start_1" `isInfixOf` str -> sstr str fl
                         _ | "result_1" `isInfixOf` str -> sstr str fl
                         _ -> Nothing
                       ) $ M.toList fls
  where
    sstr str fl = Just $ unwords [str, ":", if fl /= fl
                                            then "NaN"
                                            else show fl --(round fl :: Integer)
                                 ]

showNanResult :: String -> SMTResult -> IO ()
showNanResult str result = error $ str ++ "\n" ++ (unlines $ getNanList $ example result)

getNanList :: M.Map String Double -> [String]
getNanList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "undef" `isInfixOf` str -> Nothing
                         _ | "left_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "copy_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "right_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "start_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "start_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "start_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "result_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "right_1" `isInfixOf` str -> sstr str fl
                         _ | "isNan" `isInfixOf` str -> sstr str fl
                         _ | "jsSign" `isInfixOf` str -> sstr str fl
                         _ | "start_1" `isInfixOf` str -> sstr str fl
                         _ | "result_1" `isInfixOf` str -> sstr str fl
                         _ | "left_1" `isInfixOf` str -> sstr str fl
                         _ | "result_" `isInfixOf` str -> sstr str fl
                         _ -> Nothing
                       ) $ M.toList fls
  where
    sstr str fl = Just $ unwords [str, ":", if fl /= fl
                                            then "NaN"
                                            else show fl --(round fl :: Integer)
                                 ]

showInfResult :: String -> SMTResult -> IO ()
showInfResult str result = error $ str ++ "\n" ++ (unlines $ getInfList $ example result)

getInfList :: M.Map String Double -> [String]
getInfList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "undef" `isInfixOf` str -> Nothing
                         _ | "abs_res" `isInfixOf` str -> sstr str fl
                         _ | "abs_after" `isInfixOf` str -> sstr str fl
                         _ | "left_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "right_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "start_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "result_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "right_1" `isInfixOf` str -> sstr str fl
                         _ | "right_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "right_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "left_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "left_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "result_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "result_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "left_1" `isInfixOf` str -> sstr str fl
                         _ | "start_1" `isInfixOf` str -> sstr str fl
                         _ | "result_1" `isInfixOf` str -> sstr str fl
                         _ -> Nothing
                       ) $ M.toList fls
  where
    sstr str fl = Just $ unwords [str, ":", if isInfinite fl
                                            then "Inf"
                                            else show (round fl :: Integer)
                                 ]

showNegzResult :: String -> SMTResult -> IO ()
showNegzResult str result = error $ str ++ "\n" ++ (unlines $ getNegzList $ example result)

getNegzList :: M.Map String Double -> [String]
getNegzList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "undef" `isInfixOf` str -> Nothing
                         _ | "sbs" `isInfixOf` str -> sstr str fl
                         _ | "left_range_canBeNegativeZero" `isInfixOf` str -> sstr str fl
                         _ | "right_range_canBeNegativeZero" `isInfixOf` str -> sstr str fl
                         _ | "start_range_canBeNegativeZero" `isInfixOf` str -> sstr str fl
                         _ | "left_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "right_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "start_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "left_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "right_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "start_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "result_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "result_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "result_range_canBeNegativeZero" `isInfixOf` str -> sstr str fl
                         _ | "start_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "start_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "result_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "result_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "left_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "left_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "right_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "right_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "right_1" `isInfixOf` str -> sstr str fl
                         _ | "left_1" `isInfixOf` str -> sstr str fl
                         _ | "start_1" `isInfixOf` str -> sstr str fl
                         _ | "result_1" `isInfixOf` str -> sstr str fl
                         _ | "jsCeilStart" `isInfixOf` str -> sstr str fl
                         _ | "jsCeilResult" `isInfixOf` str -> sstr str fl
                         _ | "jsSign" `isInfixOf` str -> sstr str fl
                         _ | "jsSignStart" `isInfixOf` str -> sstr str fl
                         _ -> Nothing
                     ) $ M.toList fls
  where
    sstr str fl = Just $ unwords [str, ":", if isNegativeZero fl
                                            then "negz"
                                            else show fl
                                 ]

showFractResult :: String -> SMTResult -> IO ()
showFractResult str result = error $ str ++ "\n" ++ (unlines $ getFractList $ example result)

getFractList :: M.Map String Double -> [String]
getFractList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "undef" `isInfixOf` str -> Nothing
                         _ | "result_fract" `isInfixOf` str -> sstr str fl
                         _ | "result_1" `isInfixOf` str -> sstr str fl
                         _ | "left_range_canHaveFractionalPart" `isInfixOf` str -> sstr str fl
                         _ | "right_range_canHaveFractionalPart" `isInfixOf` str -> sstr str fl
                         _ | "start_range_canHaveFractionalPart" `isInfixOf` str -> sstr str fl
                         _ | "result_range_canHaveFractionalPart" `isInfixOf` str -> sstr str fl
                         _ | "right_1" `isInfixOf` str -> sstr str fl
                         _ | "left_1" `isInfixOf` str -> sstr str fl
                         _ | "start_1" `isInfixOf` str -> sstr str fl
                         _ | "result_1" `isInfixOf` str -> sstr str fl
                         _ -> Nothing
                     ) $ M.toList fls
  where
    sstr str fl = Just $ unwords [str, ":", if isNegativeZero fl
                                            then "negz"
                                            else show fl
                                 ]

showInt32Result :: String -> SMTResult -> IO ()
showInt32Result str result =
  case result of
    SolverSat e ->
      let list = unlines $ getIntList $ e
      in error $ str ++ "\n" ++ list
    SolverUnsat -> error "Unsat result"
    SolverFailed -> error "Solver failed (e.g., due to timeout)"

getIntList :: M.Map String Double -> [String]
getIntList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "undef" `isInfixOf` str -> Nothing
                         _ | "l_1"  `isInfixOf` str -> sstr str fl
                         _ | "elem" `isInfixOf` str -> sstr str fl
                         _ | "subResult" `isInfixOf` str -> sstr str fl
                         _ | "shift" `isInfixOf` str -> sstr str fl
                         _ | "startIsUndef" `isInfixOf` str -> sstr str fl
                         _ | "left_range_canHaveFractionalPart" `isInfixOf` str -> sstr str fl
                         _ | "right_range_canHaveFractionalPart" `isInfixOf` str -> sstr str fl
                         _ | "result_range_canHaveFractionalPart" `isInfixOf` str -> sstr str fl
                         _ | "left_range_isEmpty" `isInfixOf` str -> sstr str fl
                         _ | "right_range_isEmpty" `isInfixOf` str -> sstr str fl
                         _ | "result_range_isEmpty" `isInfixOf` str -> sstr str fl
                         _ | "isInLeft" `isInfixOf` str -> sstr str fl
                         _ | "isInRight" `isInfixOf` str -> sstr str fl
                         _ | "isInResult" `isInfixOf` str -> sstr str fl
                         _ | "left_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "right_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "result_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "range_inter" `isInfixOf` str -> sstr str fl
                         _ | "result_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "result_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "result_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "result_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "right_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "right_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "left_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "left_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "left_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "left_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "right_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "right_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "start_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "start_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "result_1" `isInfixOf` str -> sstr str fl
                         _ | "right_1" `isInfixOf` str -> sstr str fl
                         _ | "left_1" `isInfixOf` str -> sstr str fl
                         _ | "start_" `isInfixOf` str -> sstr str fl
                         _ | "left_range_inter_lower" `isInfixOf` str -> sstr str fl
                         _ | "right_range_inter_lower" `isInfixOf` str -> sstr str fl
                         _ | "left_range_inter_upper" `isInfixOf` str -> sstr str fl
                         _ | "right_range_inter_upper" `isInfixOf` str -> sstr str fl
                         _ | "start_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "start_range_upper" `isInfixOf` str -> sstr str fl
                         _ -> Nothing
                     ) $ M.toList fls
  where
    sstr str fl = Just $ unwords [str, ":", show fl] --show (round fl :: Integer)]

prettyCounterexampleInts :: M.Map String Double
                         -> String
prettyCounterexampleInts ce = unlines $ getIntList ce
