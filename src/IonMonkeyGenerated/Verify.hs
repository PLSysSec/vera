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
import           GHC.Float
import           IonMonkeyGenerated.Helpers
import           IonMonkeyGenerated.Objects
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
  define canBeInfiniteOrNan
  define setLowerInit
  define setUpperInit
  define range3
  define range6
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
  define verifyNan
  define verifyInf
  define verifyExp
  define canBeInfiniteOrNan
  define setLowerInit
  define setUpperInit
  define range3
  define range6
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
              , declare (t Double) "left"
              , declare (c "range") "right_range"
              , declare (t Double) "right"
              , declare (c "range") "result_range"
              , declare (t Double) "result"
              , (v "left_range")   `assign` (call "newFloatInputRange" [])
              , (v "right_range")  `assign` (call "newFloatInputRange" [])
              , (v "result_range") `assign` call fnName [v "left_range", v "right_range"]
                -- Verify that the result range is well formed
        --      , vcall "verifySaneRange" [v "result_range"]
                -- Actually perform the JS operation
              , (v "left")  `assign` (call "floatInRange" [v "left_range"])
              , (v "right") `assign` (call "floatInRange" [v "right_range"])
              , (v "result") `assign` (v "left" `jsOp` v "right")
                -- Verify FP properties
              , vcall "verifyNegZ" [v "result_range", v "result"]
              , vcall "verifyNan"  [v "result_range", v "result"]
              , vcall "verifyInf"  [v "result_range", v "result"]
--              , vcall "verifyExp"  [v "result_range", v "result"]
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
  define setUpperInit
  define range3
  define range6
  define range4
  define canBeFiniteNonNegative
  define exponentImpliedByInt32Bounds
  define numBits
  define canBeNan
  define canBeZero
  define contains
  define hasInt32Bounds
  define missingAnyInt32Bounds
  define canHaveSignBitSet
  forM_ fns define
  let verif = [ declare (c "range") "start_range"
              , declare (t Double) "start"
              , declare (c "range") "result_range"
              , declare (t Double) "result"
              , (v "start_range")  `assign` (call "newFloatInputRange" [])
              , (v "result_range") `assign` call fnName [v "start_range"]
                -- Verify that the result range is well formed
        --      , vcall "verifySaneRange" [v "result_range"]
                -- Actually perform the JS operation
              , (v "start")  `assign` (call "floatInRange" [v "start_range"])
              , (v "result") `assign` (jsOp $ v "start")
                -- Verify FP properties
              , vcall "verifyNegZ" [v "result_range", v "result"]
              , vcall "verifyNan"  [v "result_range", v "result"]
              , vcall "verifyInf"  [v "result_range", v "result"]
--              , vcall "verifyExp"  [v "result_range", v "result"]
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
  let args = [ ("result_range_init", c "range")]
      body = [ declare (t Double) "result_init"

             -- Either its infinite or the exponent is less than the infinte exponent
             , assert_ $ (isInf $ v "result_init") .^. ((v "result_range_init" .->. "maxExponent") .<. includesInfinity)

             -- Either is not inf or nan or op is inf or nan
             , assert_ $  ((isInf $ v "result_init") .||. ((isNan $ v "result_init")) .^. ((v "result_range_init" .->. "maxExponent" .<. includesInfinityAndNan)))

             -- If the range doesnt say can be neg z, cant be negz
             , assert_ $ (not_ $ v "result_range_init" .->. "canBeNegativeZero") .^. (isNeg (v "result_init") .&&. (isZero $ v "result_init") )

             -- The exponent should be >= the fpExp
             , assert_ $ (((fpExp $ v "result_init") .==. (v "result_range_init" .->. "maxExponent")) .&&. (not_ $ isInf $ v "result_init") .&&. (not_ $ isNan $ v "result_init"))

             -- If the number isnt in bounds, int32bound should be false
             , assert_ $ ((v "result_init" .=>. (cast int32min Double)) .^. (not_ $ v "result_range_init" .->. "hasInt32LowerBound")) .&&. ((v "result_init" .<=. (cast int32max Double)) .^. (not_ $ v "result_range_init" .->. "hasInt32UpperBound"))

             , return_ $ v "result_init"

             ]
  in Function "floatInRange" (t Double) args body

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
                         _ | "left_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "right_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "start_range_maxExponent" `isInfixOf` str -> sstr str fl
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

showNanResult :: String -> SMTResult -> IO ()
showNanResult str result = error $ str ++ "\n" ++ (unlines $ getNanList $ example result)

getNanList :: M.Map String Double -> [String]
getNanList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "undef" `isInfixOf` str -> Nothing
                         _ | "left_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "right_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "start_range_maxExponent" `isInfixOf` str -> sstr str fl
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
                         _ | "left_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "right_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "start_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "result_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "right_1" `isInfixOf` str -> sstr str fl
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
                         _ | "left_range_canBeNegativeZero" `isInfixOf` str -> sstr str fl
                         _ | "right_range_canBeNegativeZero" `isInfixOf` str -> sstr str fl
                         _ | "start_range_canBeNegativeZero" `isInfixOf` str -> sstr str fl
                         _ | "result_range_canBeNegativeZero" `isInfixOf` str -> sstr str fl
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
