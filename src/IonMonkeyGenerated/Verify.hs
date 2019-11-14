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

verifyUnaryFunction :: String
                    -> (Codegen SExpr -> Codegen SExpr)
                    -> [FunctionDef]
                    -> Codegen ()
verifyUnaryFunction fnName jsOp fns = do
  class_ range
  define newInt32InputRange
  define intInRange
  define verifySaneRange
  define verifyLower
  define verifyUpper
  define verifyUB
  define canBeInfiniteOrNan
  define setLowerInit
  define exponentImpliedByInt32Bounds
  define setUpperInit
  define range3
  define range6
  define range4
  define canBeFiniteNonNegative
  define numBits
  define canBeNan
  define optimize
  define canBeZero
  define contains
  define hasInt32Bounds
  define missingAnyInt32Bounds
  define canHaveSignBitSet
  forM_ fns define
  let verif = [ declare (c "range") "start_range"
              , declare (t Signed) "start"
              , declare (c "range") "result_range"
              , declare (t Signed) "result"
              , (v "start_range")   `assign` (call "newInt32InputRange" [])
              , (v "result_range") `assign` call fnName [v "start_range"]
                -- Verify that the result range is well formed
              , vcall "verifySaneRange" [v "result_range"]
                -- Actually perform the JS operation
              , (v "start")  `assign` (call "intInRange" [v "start_range"])
              , (v "result") `assign` (jsOp $ v "start")
                -- Verify that the result is in the computed range
              , vcall "verifyLower" [v "result_range", v "result"]
              , vcall "verifyUpper" [v "result_range", v "result"]
              ]
  genBodySMT verif

verifyFunctionConstArg :: String
                       -> (Codegen SExpr -> Codegen SExpr -> Codegen SExpr)
                       -> [FunctionDef]
                       -> Codegen ()
verifyFunctionConstArg fnName jsOp fns = do
  class_ range
  define newInt32InputRange
  define optimize
  define intInRange
  define exponentImpliedByInt32Bounds
  define verifySaneRange
  define verifyLower
  define verifyUpper
  define verifyUB
  forM_ fns define
  let verif = [ declare (c "range") "left_range"
              , declare (t Signed) "left"
              , declare (t Signed) "right"
              , declare (c "range") "result_range"
              , declare (t Signed) "result"
              , (v "left_range")   `assign` (call "newInt32InputRange" [])
              , (v "result_range") `assign` call fnName [v "left_range", v "right"]
                -- Verify that the result range is well formed
              , vcall "verifySaneRange" [v "result_range"]
                -- Actually perform the JS operation
              , (v "left")  `assign` (call "intInRange" [v "left_range"])
              , (v "result") `assign` (v "left" `jsOp` v "right")
                -- Verify that the result is in the computed range
              , vcall "verifyLower" [v "result_range", v "result"]
              , vcall "verifyUpper" [v "result_range", v "result"]
              ]
  genBodySMT verif

verifyFunction :: String
               -> (Codegen SExpr -> Codegen SExpr -> Codegen SExpr)
               -> [FunctionDef]
               -> Codegen ()
verifyFunction fnName jsOp fns = do
  class_ range
  define newInt32InputRange
  define intInRange
  define verifySaneRange
  define optimize
  define isFiniteNonNegative
  define verifyLower
  define verifyUpper
  define verifyUB
  define canBeInfiniteOrNan
  define setLowerInit
  define setUpperInit
  define range3
  define range6
  define exponentImpliedByInt32Bounds
  define range4
  define canBeFiniteNonNegative
  define numBits
  define canBeNan
  define canBeZero
  define contains
  define hasInt32Bounds
  define missingAnyInt32Bounds
  define canHaveSignBitSet
  forM_ fns define
  let verif = [ declare (c "range") "left_range"
              , declare (t Signed) "left"
              , declare (c "range") "right_range"
              , declare (t Signed) "right"
              , declare (c "range") "result_range"
              , declare (t Signed) "result"
              , (v "left_range")   `assign` (call "newInt32InputRange" [])
              , (v "right_range")  `assign` (call "newInt32InputRange" [])
              , (v "result_range") `assign` call fnName [v "left_range", v "right_range"]
                -- Verify that the result range is well formed
              , vcall "verifySaneRange" [v "result_range"]
                -- Actually perform the JS operation
              , (v "left")  `assign` (call "intInRange" [v "left_range"])
              , (v "right") `assign` (call "intInRange" [v "right_range"])
              , (v "result") `assign` (v "left" `jsOp` v "right")
                -- Verify that the result is in the computed range
              , vcall "verifyLower" [v "result_range", v "result"]
              , vcall "verifyUpper" [v "result_range", v "result"]
              ]
  genBodySMT verif

verifyFpFunction :: String
                 -> (Codegen SExpr -> Codegen SExpr -> Codegen SExpr)
                 -> [FunctionDef]
                 -> Codegen ()
verifyFpFunction fnName jsOp fns = do
  class_ range
  define floatInRange
  define newFloatInputRange
  define verifySaneRange
  define verifyNegZero
  define optimize
  define verifyNan
  define verifyFpBound
  define verifyInf
  define verifyExp
  define verifyLowBoundInvariant
  define verifyUpBoundInvariant
  define canBeInfiniteOrNan
  define setLowerInit
  define setUpperInit
  define range3
  define range6
  define exponentImpliedByInt32Bounds
  define range4
  define verifyFract
  define canBeFiniteNonNegative
  define numBits
  define canBeNan
  define canBeZero
  define contains
  define hasInt32Bounds
  define missingAnyInt32Bounds
  define canHaveSignBitSet
  forM_ fns define
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
              , (v "result") `assign` (v "left" `jsOp` v "right")
              , expect_ isSat (error "Has to start out SAT")
              --   -- Verify FP properties
              , vcall "verifyFpBound"    [v "result_range", v "result"]
              , vcall "verifyLowBoundInvariant"   [v "result_range", v "result"]
              , vcall "verifyUpBoundInvariant"   [v "result_range", v "result"]
              , vcall "verifyNegZ"   [v "result_range", v "result"]
              , vcall "verifyNan"    [v "result_range", v "result"]
              , vcall "verifyInf"    [v "result_range", v "result"]
              , vcall "verifyFract"  [v "result_range", v "result"]
              , vcall "verifyExp"    [v "result_range", v "result"]
              ]
  genBodySMT verif

verifyFpUnaryFunction :: String
                      -> (Codegen SExpr -> Codegen SExpr)
                      -> [FunctionDef]
                      -> Codegen ()
verifyFpUnaryFunction fnName jsOp fns = do
  class_ range
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
  define contains
  define hasInt32Bounds
  define missingAnyInt32Bounds
  define verifyLowBoundInvariant
  define verifyUpBoundInvariant
  define canHaveSignBitSet
  define verifyFract
  forM_ fns define
  let verif = [ declare (c "range") "start_range"
              , declare (t Double) "start"
              , declare (c "range") "result_range"
              , declare (t Double) "result_verif"
              , (v "start_range")  `assign` (call "newFloatInputRange" [])
              , (v "result_range") `assign` call fnName [v "start_range"]
                -- Verify that the result range is well formed
        --      , vcall "verifySaneRange" [v "result_range"]
                -- Actually perform the JS operation
              , (v "start")  `assign` (call "floatInRange" [v "start_range"])
              , (v "result_verif") `assign` (jsOp $ v "start")
                -- Verify FP properties
              , vcall "verifyFpBound"    [v "result_range", v "result_verif"]
              , vcall "verifyLowBoundInvariant"   [v "result_range", v "result_verif"]
              , vcall "verifyUpBoundInvariant"   [v "result_range", v "result_verif"]
              , vcall "verifyNegZ"   [v "result_range", v "result_verif"]
              , vcall "verifyNan"    [v "result_range", v "result_verif"]
              , vcall "verifyInf"    [v "result_range", v "result_verif"]
              , vcall "verifyFract"  [v "result_range", v "result_verif"]
              , vcall "verifyExp"    [v "result_range", v "result_verif"]
              ]
  genBodySMT verif

verifyMetaUnion :: Codegen ()
verifyMetaUnion = do
  class_ range
  define floatIsInRange
  define verifyUnion
  define verifyIntersection
  define union
  define intersect
  define newFloatInputRange
  let verif = [ declare (c "range") "left_range"
              , declare (c "range") "right_range"
              , declare (c "range") "result_range_union"
              , declare (c "range") "result_range_intersection"
              , (v "left_range")   `assign` (call "newFloatInputRange" [])
              , (v "right_range")  `assign` (call "newFloatInputRange" [])
              , (v "result_range_union") `assign` call "union" [v "left_range", v "right_range"]
              -- , (v "result_range_intersection") `assign` call "intersect" [v "left_range", v "right_range"]
              , expect_ isSat (error "Has to start out SAT")
              -- , vcall "verifyUnion" [ v "right_range"
              --                       , v "left_range"
              --                       , v "result_range_union"
              --                       ]
              , vcall "verifyIntersection" [ v "left_range"
                                           , v "right_range"
                                           , v "result_range_intersection"
                                           ]
              ]
  genBodySMT verif

-- Setup

intInRange :: FunctionDef
intInRange =
  let args = [ ("result_range_init", c "range")]
      body = [ declare (t Signed) "result_init"
             , assert_ $ (v "result_init") .=>. ((v "result_range_init") .->. "lower")
             , assert_ $ (v "result_init") .<=. ((v "result_range_init") .->. "upper")
             , assert_ $ not_ $ undef $ v "result_init"
             , return_ $ v "result_init"
             ]
  in Function "intInRange" (t Signed) args body

floatInRange :: FunctionDef
floatInRange =
  let args = [ ("result_range_init", c "range")]
      body = [ declare (t Double) "result_init"

             -- |v| == OO   ==  exp >= includesInfinity
             , implies_ (isInf $ v "result_init") ((v "result_range_init" .->. "maxExponent") .=>. includesInfinity)

             -- isNan(v)    ==  exp == includesInfinityAndNan
             , implies_ (isNan $ v "result_init") ((v "result_range_init" .->. "maxExponent") .==. includesInfinityAndNan)

             -- isNegZero(v)  => canBeNegativeZero
             , implies_ (isNegZero $ v "result_init") (v "result_range_init" .->. "canBeNegativeZero")

             -- v != round(v)  => canHaveFractionalPart
             , implies_ (not_ $ v "result_init" .==. (jsCeil $ v "result_init")) (v "result_range_init" .->. "canHaveFractionalPart")

             -- ((!Nan(v) and !Inf(v)) => log2(v) <= exp,
             , declare (t Bool) "goodExpBoundHigh"
             , declare (t Bool) "goodExpBoundLow"

             , v "goodExpBoundLow" `assign` ((fpExp $ v "result_init") .<=. (v "result_range_init" .->. "maxExponent"))

             , v "goodExpBoundHigh" `assign` (((fpExp $ v "result_init") .+. n Unsigned16 1) .>. (v "result_range_init" .->. "maxExponent"))

             -- , v "goodExpBoundHigh" `assign` (( (fpExp $ cast (max_ (abs_ $ v "result_range_init" .->. "lower") (abs_ $ v "result_range_init" .->. "upper")) Double) .+. n Signed16 1) .>. (v "result_range_init" .->. "maxExponent"))

             , implies_ ((not_ $ isNan $ v "result_init") .&&. (not_ $ isInf $ v "result_init")) (v "goodExpBoundHigh" .&&. v "goodExpBoundLow")

             -- BOUNDS ASSERTIONS: HARDEST

             -- !hasInt32LBound => lower = jsMin
            , implies_ (not_ $ v "result_range_init" .->. "hasInt32LowerBound") (v "result_range_init" .->. "lower" .==. jsIntMin)

             -- !hasInt32HBound => upper = jsMax
            , implies_ (not_ $ v "result_range_init" .->. "hasInt32UpperBound") (v "result_range_init" .->. "upper" .==. jsIntMax)

              -- optimized negz
              , implies_ (not_ $ call "contains" [v "result_range_init", n Signed 0]) (not_ $ v "result_range_init" .->. "canBeNegativeZero")

              -- lower and upper are both within int min and int max
              -- lower is less than upper
             , assert_ $ (v "result_range_init" .->. "lower" .=>. jsIntMin) .&&. (v "result_range_init" .->. "lower" .<=. jsIntMax)
             , assert_ $ (v "result_range_init" .->. "upper" .=>. jsIntMin) .&&. (v "result_range_init" .->. "upper" .<=. jsIntMax)
             , assert_ $ (v "result_range_init" .->. "lower") .<=. (v "result_range_init" .->. "upper")

             -- Special values or v < lower => !hasInt32LowerBound
             , implies_ ((isNan $ v "result_init") .||. (isInf $ v "result_init") .||. (v "result_init" .<. (cast (v "result_range_init" .->. "lower") Double))) (not_ $ v "result_range_init" .->. "hasInt32LowerBound")

             -- Special values or v > upper => !hasInt32UpperBound
             , implies_ ((isNan $ v "result_init") .||. (isInf $ v "result_init") .||. (v "result_init" .>. (cast (v "result_range_init" .->. "upper") Double))) (not_ $ v "result_range_init" .->. "hasInt32UpperBound")

             -- -- -- hasInt32Lower => v >= lower
             -- , implies_ (v "result_range_init" .->. "hasInt32LowerBound") ((v "result_init") .=>. (cast (v "result_range_init" .->. "lower") Double))

             -- -- hasInt32Upper => v <= upper
             -- , implies_ (v "result_range_init" .->. "hasInt32UpperBound") ((v "result_init") .<=. (cast (v "result_range_init" .->. "upper") Double))

             , return_ $ v "result_init"

             ]
  in Function "floatInRange" (t Double) args body

floatIsInRange :: FunctionDef
floatIsInRange =
  let args = [ ("result_range_init", c "range")
             , ("result_init", t Double)
             ]
      body = [ declare (t Bool) "inRange"
             , declare (t Bool) "infHolds"

             , if_ (v "result_range_init" .->. "isEmpty")
               [return_ $ n Bool 0] []

             , declare (t Bool) "infHolds"
             , v "infHolds" `assign` (testImplies (isInf $ v "result_init") ((v "result_range_init" .->. "maxExponent") .=>. includesInfinity))

             , declare (t Bool) "nanHolds"
             , v "nanHolds" `assign` (testImplies (isNan $ v "result_init") ((v "result_range_init" .->. "maxExponent") .==. includesInfinityAndNan))

             , declare (t Bool) "negzHolds"
             , v "negzHolds" `assign` (testImplies (isNegZero $ v "result_init") (v "result_range_init" .->. "canBeNegativeZero"))

             , declare (t Bool) "fractHolds"
             , v "fractHolds" `assign` (testImplies (not_ $ v "result_init" .==. (jsCeil $ v "result_init")) (v "result_range_init" .->. "canHaveFractionalPart"))

             , declare (t Bool) "expHolds"
             , declare (t Bool) "goodExpVal"
             , declare (t Bool) "goodExpBound"
             , declare (t Bool) "goodExpBound2"
             , v "goodExpVal" `assign` ((fpExp $ v "result_init") .<=. (v "result_range_init" .->. "maxExponent"))
             , v "goodExpBound" `assign` (( (fpExp $ cast (max_ (abs_ $ v "result_range_init" .->. "lower") (abs_ $ v "result_range_init" .->. "upper")) Double) .+. n Signed16 1) .>. (v "result_range_init" .->. "maxExponent"))
             , v "goodExpBound2" `assign` (( (fpExp $ cast (max_ (abs_ $ v "result_range_init" .->. "lower") (abs_ $ v "result_range_init" .->. "upper")) Double)) .<=. (v "result_range_init" .->. "maxExponent"))
             , v "expHolds" `assign` (testImplies ((not_ $ isNan $ v "result_init") .&&. (not_ $ isInf $ v "result_init")) (v "goodExpVal"))

             , assert_ $ (v "goodExpBound" .&&. v "goodExpBound2") .||. (v "result_range_init" .->. "maxExponent" .=>. includesInfinity)
             , implies_ (v "result_range_init" .->. "maxExponent" .=>. includesInfinity) (not_ $ v "result_range_init" .->. "hasInt32LowerBound")
             , implies_ (v "result_range_init" .->. "maxExponent" .=>. includesInfinity) (not_ $ v "result_range_init" .->. "hasInt32UpperBound")
             , assert_ $ (v "result_range_init" .->. "maxExponent" .==. includesInfinity) .||. (v "result_range_init" .->. "maxExponent" .==. includesInfinityAndNan) .||. (v "result_range_init" .->. "maxExponent" .<=. maxFiniteExponent)

            -- BOUNDS ASSERTIONS ON FORMED-NESS MUST STILL HOLD
            ,  implies_ (not_ $ v "result_range_init" .->. "hasInt32LowerBound") (v "result_range_init" .->. "lower" .==. jsIntMin)
            , implies_ (not_ $ v "result_range_init" .->. "hasInt32UpperBound") (v "result_range_init" .->. "upper" .==. jsIntMax)
             , assert_ $ (v "result_range_init" .->. "lower" .=>. jsIntMin) .&&. (v "result_range_init" .->. "lower" .<=. jsIntMax)
             , assert_ $ (v "result_range_init" .->. "upper" .=>. jsIntMin) .&&. (v "result_range_init" .->. "upper" .<=. jsIntMax)
             , assert_ $ (v "result_range_init" .->. "lower") .<=. (v "result_range_init" .->. "upper")

             , declare (t Bool) "hasLowHolds"
             , v "hasLowHolds" `assign` (testImplies ((isNan $ v "result_init") .||. (isInf $ v "result_init") .||. (v "result_init" .<. (cast (v "result_range_init" .->. "lower") Double))) (not_ $ v "result_range_init" .->. "hasInt32LowerBound"))

             , declare (t Bool) "hasHighHolds"
             , v "hasHighHolds" `assign` (testImplies ((isNan $ v "result_init") .||. (isInf $ v "result_init") .||. (v "result_init" .>. (cast (v "result_range_init" .->. "upper") Double))) (not_ $ v "result_range_init" .->. "hasInt32UpperBound"))

             , return_ $ v "hasHighHolds" .&&. v "hasLowHolds" .&&. v "expHolds" .&&. v "fractHolds" .&&. v "negzHolds" .&&. v "nanHolds" .&&. v "infHolds"

             ]
  in Function "floatIsInRange" (t Bool) args body

-- Verification

-- Union and intersection

verifyUnion :: FunctionDef
verifyUnion =
  let args = [ ("right_range_union", c "range")
             , ("left_range_union", c "range")
             , ("result_range_union", c "range")
             ]
      body = [ declare (t Double) "elem"
             , push_
             , declare (t Bool) "in_right"
             , declare (t Bool) "in_left"
             , declare (t Bool) "in_result"
             , v "in_right" `assign` call "floatIsInRange" [v "right_range_union", v "elem"]
             , v "in_left" `assign` call "floatIsInRange" [v "left_range_union",  v "elem"]
             , v "in_result" `assign` call "floatIsInRange" [v "result_range_union", v "elem"]
             , assert_ $ ((v "in_right" .||. v "in_left") .&&. (not_ $ v "in_result")) .&&. (not_ $ v "result_range_union" .->. "isEmpty")
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify union" r
             , pop_
             ]
  in Function "verifyUnion" Void args body

verifyIntersection :: FunctionDef
verifyIntersection =
  let args = [ ("right_range_intersect", c "range")
             , ("left_range_intersect", c "range")
             , ("result_range_intersect", c "range")
             ]
      body = [ declare (t Double) "elem"
             , push_
             , declare (t Bool) "in_right"
             , declare (t Bool) "in_left"
             , declare (t Bool) "in_result"
             , v "in_right" `assign` call "floatIsInRange" [v "right_range_intersect", v "elem"]
             , v "in_left" `assign` call "floatIsInRange" [v "left_range_intersect",  v "elem"]
             , v "in_result" `assign` call "floatIsInRange" [v "result_range_intersect", v "elem"]
             , assert_ $ ((v "in_right" .&&. v "in_left") .&&. (not_ $ v "in_result")) .&&. (not_ $ v "result_range_intersect" .->. "isEmpty")
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify union" r
             , pop_
             ]
  in Function "verifyIntersection" Void args body

-- Normal

verifySaneRange :: FunctionDef
verifySaneRange =
  let args = [ ("result_range_s", c "range")]
      body = [ push_
             , assert_ $ (v "result_range_s") .->. "hasInt32LowerBound"
             , assert_ $ (v "result_range_s") .->. "hasInt32UpperBound"
             , assert_ $ ((v "result_range_s") .->. "lower") .>. ((v "result_range_s") .->. "upper")
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify sane range" r
             , pop_
             ]
  in Function "verifySaneRange" Void args body

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

verifyUB :: FunctionDef
verifyUB =
  let args = [ ("result_range_undef", c "range")
             , ("result_undef", t Signed)
             ]
      body = [ push_
             , assert_ $ undef $ v "result_undef"
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify upper" r
             , pop_
             ]
  in Function "verifyUB" Void args body

-- Floating point

verifyLowBoundInvariant :: FunctionDef
verifyLowBoundInvariant =
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
             ]
  in Function "verifyLowBoundInvariant" Void args body

verifyUpBoundInvariant :: FunctionDef
verifyUpBoundInvariant =
  let args = [ ("result_range_biu", c "range")
             , ("result_biu", t Double)
             ]
      body = [ push_
              -- It has no int32 lower bound...
             , assert_ $ not_ $ v "result_range_biu" .->. "hasInt32UpperBound"
               -- ...but the int32 lower bound isnt intmax
             , assert_ $ not_ $ (v "result_range_biu" .->. "upper") .==. jsIntMax
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify high bound invariant" r
             , pop_
             ]
  in Function "verifyUpBoundInvariant" Void args body

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

verifyNan :: FunctionDef
verifyNan =
  let args = [ ("result_range_nan", c "range")
             , ("result_nan", t Double)
             ]
      body = [ push_
               -- It's Nan
             , declare (t Bool) "isNan"
             , v "isNan" `assign` (isNan $ v "result_nan")
             , assert_ $ isNan $ v "result_nan"
               -- ... but the Nan exponent is not correct
             , assert_ $ not_ $ (v "result_range_nan" .->. "maxExponent") .==. includesInfinityAndNan
             , expect_ isUnsat $ \r -> showNanResult "Failed to verify Nan" r
             , pop_
             ]
  in Function "verifyNan" Void args body

verifyInf :: FunctionDef
verifyInf =
  let args = [ ("result_range_inf", c "range")
             , ("result_inf", t Double)
             ]
      body = [ push_
               -- It's inf
             , assert_ $ isInf $ v "result_inf"
               -- ... but the inf exponent is not correct
             , assert_ $ not_ $ (v "result_range_nan" .->. "maxExponent") .=>. includesInfinity
             , expect_ isUnsat $ \r -> showInfResult "Failed to verify Inf" r
             , pop_
             ]
  in Function "verifyInf" Void args body

verifyExp :: FunctionDef
verifyExp =
  let args = [ ("result_range_exp", c "range")
             , ("result_exp", t Double)
             ]
      body = [ push_
             , assert_ $ (fpExp $ v "result_exp") .>. (v "result_range_exp" .->. "maxExponent")
             , expect_ isUnsat $ \r -> showExpResult "Failed to verify Exp" r
             , pop_
             ]
  in Function "verifyExp" Void args body

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

verifyFpBound :: FunctionDef
verifyFpBound =
  let args = [ ("result_range_fpbound", c "range")
             , ("result_fpbound", t Double)
             ]
      body = [ push_
             , assert_ $ ((v "result_range_fpbound" .->. "lower") .>. (v "result_range_fpbound" .->. "upper"))
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify fp bound" r
             , pop_
             ]
  in Function "verifyFpBound" Void args body

-- Copypasted

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
                         _ | "copy_maxExponent" `isInfixOf` str -> sstr str fl
                         -- _ | "eib_ret" `isInfixOf` str -> sstr str fl
                         _ | "themax" `isInfixOf` str -> sstr str fl
                         _ | "abs_lower" `isInfixOf` str -> sstr str fl
                         _ | "abs_upper" `isInfixOf` str -> sstr str fl
                         _ | "la_" `isInfixOf` str -> sstr str fl
                         _ | "ua_" `isInfixOf` str -> sstr str fl
                         _ | "upper_big" `isInfixOf` str -> sstr str fl
                         _ | "start_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "start_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         -- _ | "start_range_canHaveFractionalPart" `isInfixOf` str -> sstr str fl
                         _ | "start_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "start_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "start_1" `isInfixOf` str -> sstr str fl
                         -- _ | "result_range_upper" `isInfixOf` str -> sstr str fl
                         -- _ | "abs_res" `isInfixOf` str -> sstr str fl
                         -- _ | "abs_after" `isInfixOf` str -> sstr str fl
                         _ | "result_verif_1" `isInfixOf` str -> sstr str fl
                         -- _ | "left_range_maxExponent" `isInfixOf` str -> sstr str fl
                         -- _ | "right_range_maxExponent" `isInfixOf` str -> sstr str fl
                         -- _ | "start_range_maxExponent" `isInfixOf` str -> sstr str fl
                         -- _ | "result_range_maxExponent" `isInfixOf` str -> sstr str fl
                         -- _ | "right_1" `isInfixOf` str -> sstr str fl
                         -- _ | "left_1" `isInfixOf` str -> sstr str fl
                         -- _ | "start_1" `isInfixOf` str -> sstr str fl
                         -- _ | "result_1" `isInfixOf` str -> sstr str fl
                         _ -> Nothing
                       ) $ M.toList fls
  where
    sstr str fl = Just $ unwords [str, ":", if fl /= fl
                                            then "NaN"
                                            else show (round fl :: Integer)
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
                         _ | "left_1" `isInfixOf` str -> sstr str fl
                         _ | "start_1" `isInfixOf` str -> sstr str fl
                         _ | "result_1" `isInfixOf` str -> sstr str fl
                         _ -> Nothing
                       ) $ M.toList fls
  where
    sstr str fl = Just $ unwords [str, ":", if fl /= fl
                                            then "NaN"
                                            else show (round fl :: Integer)
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
                         _ | "sent_in" `isInfixOf` str -> sstr str fl
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
showInt32Result str result = error $ str ++ "\n" ++ (unlines $ getIntList $ example result)

getIntList :: M.Map String Double -> [String]
getIntList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "undef" `isInfixOf` str -> Nothing
                         -- _ | "result_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "in_right" `isInfixOf` str -> sstr str fl
                         _ | "in_left" `isInfixOf` str -> sstr str fl
                         _ | "in_result" `isInfixOf` str -> sstr str fl
                         _ | "elem" `isInfixOf` str -> sstr str fl
                         _ | "left_range_intersect" `isInfixOf` str -> sstr str fl
                         _ | "right_range_intersect" `isInfixOf` str -> sstr str fl
                         _ | "result_range_intersect" `isInfixOf` str -> sstr str fl
                         -- _ | "result_range_lower" `isInfixOf` str -> sstr str fl
                         -- _ | "result_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         -- _ | "result_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         -- _ | "result_range_union_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         -- _ | "result_range_union_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         -- _ | "result_range_intersection_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         -- _ | "result_range_intersection_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         -- _ | "left_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         -- _ | "left_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         -- _ | "right_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         -- _ | "right_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         -- _ | "start_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         -- _ | "start_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         -- _ | "result_1" `isInfixOf` str -> sstr str fl
                         -- _ | "right_1" `isInfixOf` str -> sstr str fl
                         -- _ | "left_1" `isInfixOf` str -> sstr str fl
                         -- _ | "start_1" `isInfixOf` str -> sstr str fl
                         -- _ | "left_range_lower" `isInfixOf` str -> sstr str fl
                         -- _ | "right_range_lower" `isInfixOf` str -> sstr str fl
                         -- _ | "left_range_upper" `isInfixOf` str -> sstr str fl
                         -- _ | "right_range_upper" `isInfixOf` str -> sstr str fl
                         -- _ | "start_range_lower" `isInfixOf` str -> sstr str fl
                         -- _ | "start_range_upper" `isInfixOf` str -> sstr str fl
                         _ -> Nothing
                     ) $ M.toList fls
  where
    sstr str fl = Just $ unwords [str, ":", show fl] --show (round fl :: Integer)]

prettyCounterexampleInts :: M.Map String Double
                         -> String
prettyCounterexampleInts ce = unlines $ getIntList ce
