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
              , (v "start_range")   `assign` (call "newIn32InputRange" [])
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

-- Call these to just do all the checks
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
              , (v "left_range")   `assign` (call "newIn32InputRange" [])
              , (v "right_range")  `assign` (call "newIn32InputRange" [])
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

-- Setup

intInRange :: FunctionDef
intInRange =
  let args = [ ("result_range", c "range")]
      body = [ declare (t Signed) "result"
             , assert_ $ (v "result") .=>. ((v "result_range") .->. "lower")
             , assert_ $ (v "result") .<=. ((v "result_range") .->. "upper")
             , return_ $ v "result"
             ]
  in Function "intInRange" (t Signed) args body

-- Verification

verifySaneRange :: FunctionDef
verifySaneRange =
  let args = [ ("result_range", c "range")]
      body = [ push_
             , assert_ $ (v "result_range") .->. "hasInt32LowerBound"
             , assert_ $ (v "result_range") .->. "hasInt32UpperBound"
             , assert_ $ ((v "result_range") .->. "lower") .>. ((v "result_range") .->. "upper")
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify sane range" r
             , pop_
             ]
  in Function "verifySaneRange" Void args body

verifyLower :: FunctionDef
verifyLower =
  let args = [ ("result_range", c "range")
             , ("result", t Signed)
             ]
      body = [ push_
             , assert_ $ (v "result_range") .->. "hasInt32LowerBound"
             , assert_ $ ((v "result_range") .->. "lower") .>. (v "result")
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify lower" r
             , pop_
             ]
  in Function "verifyLower" Void args body

verifyUpper :: FunctionDef
verifyUpper =
  let args = [ ("result_range", c "range")
             , ("result", t Signed)
             ]
      body = [ push_
             , assert_ $ (v "result_range") .->. "hasInt32UpperBound"
             , assert_ $ ((v "result_range") .->. "upper") .<. (v "result")
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify upper" r
             , pop_
             ]
  in Function "verifyUpper" Void args body

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
