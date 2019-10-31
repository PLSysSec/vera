module IonMonkeyGenerated.Verify where
import           Data.List     (isInfixOf)
import qualified Data.Map      as M
import           Data.Maybe    (catMaybes)
import           DSL.Typed     (Type (..))
import           Generate.Lang

verifySaneRange :: FunctionDef
verifySaneRange =
  let args = [ ("result_range", c "range")]
      body = [ assert_ $ (v "result_range") .->. "hasInt32LowerBound"
             , assert_ $ (v "result_range") .->. "hasInt32UpperBound"
             , assert_ $ ((v "result_range") .->. "lower") .>. ((v "result_range") .->. "upper")
             , return_ $ n Signed 0
             ]
  in Function "verifySaneRange" (t Signed) args body

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

getIntList :: M.Map String Double -> [String]
getIntList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "_exp" `isInfixOf` str      -> Nothing
                         _ | "_hasFract" `isInfixOf` str -> Nothing
                         _ | "_negZero" `isInfixOf` str  -> Nothing
                         _ | "infOrNan" `isInfixOf` str  -> Nothing
                         _ -> Just $ unwords [str, ":", show (round fl :: Integer)]
                     ) $ M.toList fls

prettyCounterexampleInts :: M.Map String Double
                         -> String
prettyCounterexampleInts ce = unlines $ getIntList ce
