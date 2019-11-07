module IonMonkeyGenerated.Verify where
import           Control.Monad              (forM_)
import           Data.List                  (isInfixOf)
import qualified Data.Map                   as M
import           Data.Maybe                 (catMaybes)
import           DSL.DSL                    (isUnsat)
import           DSL.Typed                  (SMTResult (..), Type (..))
import           Generate.Lang
import           Generate.SMTAST
import           Generate.SMTGen
import           Generate.State
import           IonMonkeyGenerated.Helpers
import           IonMonkeyGenerated.Objects

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
  define intInRange
  define verifySaneRange
  define verifyLower
  define verifyUpper
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
  define verifyLower
  define verifyUpper
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
  define canBeInfiniteOrNan
  define range3
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
                -- Verify that the result range is well formed
              , vcall "verifySaneRange" [v "result_range"]
                -- Actually perform the JS operation
              , (v "left")  `assign` (call "intInRange" [v "left_range"])
              , (v "right") `assign` (call "intInRange" [v "right_range"])
              , (v "result") `assign` (v "left" `jsOp` v "right")
                -- Verify FP properties
              , vcall "verifyNegZ" [v "result_range", v "result"]
              ]
  genBodySMT verif

-- Setup

intInRange :: FunctionDef
intInRange =
  let args = [ ("result_range_init", c "range")]
      body = [ declare (t Signed) "result_init"
             , assert_ $ (v "result_init") .=>. ((v "result_range_init") .->. "lower")
             , assert_ $ (v "result_init") .<=. ((v "result_range_init") .->. "upper")
             , return_ $ v "result_init"
             ]
  in Function "intInRange" (t Signed) args body

floatInRange :: FunctionDef
floatInRange =
  let args = [ ("result_range_init", c "range")
             , ("result_init", t Double)
             ]
      body = [ declare (t Signed) "result_init"
             -- Either its infinite or the exponent is less than the infinte exponent
             , assert_ $ (isInf $ v "result_init") .^. ((v "result_range_init" .->. "maxExponent") .<. includesInfinity)
             -- Either is not inf or nan or op is inf or nan
             , assert_ $  ((isInf $ v "result_init") .||. ((isNan $ v "result_init")) .^. ((v "result_range_init" .->. "maxExponent" .<. includesInfinityAndNan)))
             -- If the range doesnt say can be neg z, cant be negz
             , assert_ $ (v "result_range_init" .->. "canBeNegativeZero") .^. (isNeg (v "result_init") .&&. (isZero $ v "result_init") )
             , return_ $ v "result_init"
             ]
  in Function "floatInRange" (t Signed) args body

-- Verification

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
             , assert_ $ ((v "result_range_u") .->. "upper") .<. (v "result_u")
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify upper" r
             , pop_
             ]
  in Function "verifyUpper" Void args body

-- Floating point

verifyNegZero :: FunctionDef
verifyNegZero =
  let args = [ ("result_range_nz", c "range")
             , ("result_nz", t Double)
             ]
      body = [ push_
               -- It's negative zero...
             , assert_ $ (isNeg $ v "result_nz") .&&. (isZero $ v "result_nz")
               -- ... but the negative zero flag isn't set
             , assert_ $ not_ $ v "result_range_nz" .->. "canBeNegativeZero"
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify NZ" r
             , pop_
             ]
  in Function "verifyNegZ" Void args body

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

getNanList :: M.Map String Double -> [String]
getNanList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "_undef" `isInfixOf` str -> Nothing
                         _ | "_hasUpperBound" `isInfixOf` str -> Nothing
                         _ | "_hasLowerBound" `isInfixOf` str -> Nothing
                         _ | "hasFract" `isInfixOf` str -> Nothing
                         _ | "negZero" `isInfixOf` str -> Nothing
                         _ -> Just $ unwords [str, ":", show (round fl :: Integer)]
                     ) $ M.toList fls

getNegzList :: M.Map String Double -> [String]
getNegzList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "_undef" `isInfixOf` str         -> Nothing
                         _ | "_hasUpperBound" `isInfixOf` str -> Nothing
                         _ | "_hasLowerBound" `isInfixOf` str -> Nothing
                         _ | "hasFract" `isInfixOf` str       -> Nothing
                         _ | "infOrNan" `isInfixOf` str       -> Nothing
                         _ -> Just $ unwords [str, ":", show (round fl :: Integer)]
                     ) $ M.toList fls

showInt32Result :: String -> SMTResult -> IO ()
showInt32Result str result = error $ str ++ "\n" ++ (unlines $ getIntList $ example result)

getIntList :: M.Map String Double -> [String]
getIntList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "undef" `isInfixOf` str -> Nothing
                         _ | "clzLhs" `isInfixOf` str -> sstr str fl
                         _ | "clzRhs" `isInfixOf` str -> sstr str fl
                         _ | "rhsUpper" `isInfixOf` str -> sstr str fl
                         _ | "rhsLeadingZeroes" `isInfixOf` str -> sstr str fl
                         _ | "result_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "result_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "result_1" `isInfixOf` str -> sstr str fl
                         _ | "right_1" `isInfixOf` str -> sstr str fl
                         _ | "left_1" `isInfixOf` str -> sstr str fl
                         _ | "start_1" `isInfixOf` str -> sstr str fl
                         _ | "left_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "right_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "left_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "right_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "start_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "start_range_upper" `isInfixOf` str -> sstr str fl
                         _ -> Nothing
                     ) $ M.toList fls
  where
    sstr str fl = Just $ unwords [str, ":", show (round fl :: Integer)]

prettyCounterexampleInts :: M.Map String Double
                         -> String
prettyCounterexampleInts ce = unlines $ getIntList ce
