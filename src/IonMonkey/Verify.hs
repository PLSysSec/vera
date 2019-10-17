module IonMonkey.Verify where
import           Control.Monad              (unless, when)
import           Control.Monad.State.Strict (liftIO)
import           Data.List                  (intersperse, isInfixOf)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (catMaybes)
import qualified DSL.DSL                    as D
import           DSL.Typed                  as T
import qualified DSL.Z3Wrapper              as D
import           IonMonkey.Helpers
import           IonMonkey.Objects

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
    show ce                    = show $ counterexample ce

getNanList :: M.Map String Double -> [String]
getNanList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "_undef" `isInfixOf` str -> Nothing
                         _ | "_hasUpperBound" `isInfixOf` str -> Nothing
                         _ | "_hasLowerBound" `isInfixOf` str -> Nothing
                         _ | "hasFract" `isInfixOf` str -> Nothing
                         _ | "negZero" `isInfixOf` str -> Nothing
                         _ -> Just $ unwords [str, ":", show $ round fl]
                     ) $ M.toList fls

getNegzList :: M.Map String Double -> [String]
getNegzList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "_undef" `isInfixOf` str         -> Nothing
                         _ | "_hasUpperBound" `isInfixOf` str -> Nothing
                         _ | "_hasLowerBound" `isInfixOf` str -> Nothing
                         _ | "hasFract" `isInfixOf` str       -> Nothing
                         _ | "infOrNan" `isInfixOf` str       -> Nothing
                         _ -> Just $ unwords [str, ":", show $ round fl]
                     ) $ M.toList fls

getIntList :: M.Map String Double -> [String]
getIntList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "_exp" `isInfixOf` str      -> Nothing
                         _ | "_hasFract" `isInfixOf` str -> Nothing
                         _ | "_negZero" `isInfixOf` str  -> Nothing
                         _ | "infOrNan" `isInfixOf` str  -> Nothing
                         _ -> Just $ unwords [str, ":", show $ round fl]
                     ) $ M.toList fls

prettyCounterexampleInts :: M.Map String Double
                         -> String
prettyCounterexampleInts ce = unlines $ getIntList ce

verifyConsistent :: D.Verif VerifResult
verifyConsistent = do
  check <- D.runSolver
  return $ case check of
    D.SolverUnsat -> UnsatImpl
    D.SolverSat{} -> Verified
    _             -> error "Error while verifying"

-- | Verify that the upper bound of a range is greater than the lower
-- Expects UNSAT
verifySaneRange :: Range -> D.Verif VerifResult
verifySaneRange resultRange = do
  D.push
  T.vassert $ hasInt32LowerBound resultRange
  T.vassert $ hasInt32UpperBound resultRange
  T.cppLt (upper resultRange) (lower resultRange) >>= T.vassert
  check <- D.runSolver
  D.pop
  return $ case check of
    D.SolverUnsat  -> Verified
    D.SolverSat xs -> OverlappingRange xs
    _              -> error "Error while verifying"

-- | Verify that a node is less than the upper bound
-- Expects UNSAT
verifyUpperBound :: T.VNode -> Range -> D.Verif VerifResult
verifyUpperBound node range = do
  D.push
  T.vassert $ hasInt32UpperBound range
  T.cppGt node (upper range) >>= T.vassert
  check <- D.runSolver
  D.pop
  return $ case check of
    D.SolverUnsat  -> Verified
    D.SolverSat xs -> BadUpperBound xs
    _              -> error "Error while verifying"

-- | Verify that a node is greater than the lower bound
-- Expects UNSAT
verifyLowerBound :: T.VNode -> Range -> D.Verif VerifResult
verifyLowerBound node range = do
  D.push
  T.vassert $ hasInt32LowerBound range
  T.cppLt node (lower range) >>= T.vassert
  check <- D.runSolver
  D.pop
  return $ case check of
    D.SolverUnsat  -> Verified
    D.SolverSat xs -> BadLowerBound xs
    _              -> error "Error while verifying"

verifyDefinedResult :: Range -> D.Verif VerifResult
verifyDefinedResult range = do
  D.push
  T.assertUndef (lower range)
  T.assertUndef (upper range)
  check <- D.runSolver
  D.pop
  return $ case check of
    D.SolverUnsat  -> Verified
    D.SolverSat xs -> UndefRange xs
    _              -> error "Error while verifying"

-- | IsNan flag set for a non-nan value?
-- | IsNan flag unset for a nan value?
verifyInfNan :: T.VNode -> Range -> D.Verif VerifResult
verifyInfNan node range = do
  nodeIsInf <- T.isInf node
  nodeIsNan <- T.isNan node
  D.push
  -- If it is infinity...
  T.vassert nodeIsInf
  -- ... maxExponent should not be less than includesInfinity
  T.cppLt (maxExponent range) includesInfinity >>= T.vassert
  check1 <- D.runSolver
  D.pop
  D.push
  -- If it is Nan, includes infinityAndNan should be true
  T.vassert nodeIsNan
  T.cppEq (maxExponent range) includesInfinityAndNan >>= T.cppNot >>= T.vassert
  check2 <- D.runSolver
  D.pop
  return $ case check1 of
    D.SolverSat xs -> NoInfFlag xs
    D.SolverFailed -> error "Error while verifying"
    D.SolverUnsat -> case check2 of
                       D.SolverUnsat  -> Verified
                       D.SolverSat xs -> NoNanFlag xs
                       _              -> error "Error while verifying"

-- | IsFract flag set for a non-fract value?
-- | IsFract flag unset for a fact value?
veriftFract :: T.VNode -> Range -> D.Verif VerifResult
veriftFract node range = error ""

-- | If the result has int32 bounds, the result should be within the specified range
-- It would be better to do this over doubles, but instead we'll cast doubles to
-- int32s so that the anlaysis has a chance at finishing.
verifyInt32Bounds :: T.VNode -> Range -> D.Verif VerifResult
verifyInt32Bounds node range = do
  -- Cap it in the range
  minNum <- T.fpnum (-2147483648)
  maxNum <- T.fpnum 2147483647
  nodeTooSmall <- T.cppLt node minNum
  nodeTooBig <- T.cppGt node maxNum
  inLowerRange <- T.cppCond nodeTooSmall minNum node
  inUpperRange <- T.cppCond nodeTooBig maxNum inLowerRange

  castValue <- T.cppCast inUpperRange T.Signed
  outOfRangeLower <- T.cppLt castValue (lower range)
  outOfRangeUpper <- T.cppGt castValue (upper range)

  let hasLowerBound = hasInt32LowerBound range
      hasUpperBound = hasInt32UpperBound range

  -- hasUpperBound && outside of upper bound should be impossible
  D.push
  T.vassert hasLowerBound
  T.vassert outOfRangeLower
  check1 <- D.runSolver
  D.pop
  case check1 of
    SolverSat xs -> return $ BadLowerBound xs
    SolverFailed -> error "Error while verifying"
    _            -> do
      -- hasLowerBound && outside of lower bound should be impossible
      D.push
      T.vassert hasUpperBound
      T.vassert outOfRangeUpper
      check2 <- D.runSolver
      D.pop
      return $ case check2 of
        SolverSat xs -> BadUpperBound xs
        SolverFailed -> error "Error while verifying"
        _            -> Verified

verifyNegZero :: T.VNode -> Range -> D.Verif VerifResult
verifyNegZero node range = do
  isZero <- T.isZero node
  isNeg <- T.isNeg node
  D.push
  -- It's negative zero...
  T.cppAnd isZero isNeg >>= T.vassert
  -- ... but the nz flag is not set
  T.cppNot (canBeNegativeZero range) >>= T.vassert
  check1 <- T.runSolver
  D.pop
  return $ case check1 of
    D.SolverSat xs -> NoNegzFlag xs
    D.SolverFailed -> error "Error while verifying"
    D.SolverUnsat  -> Verified

verifExponent :: T.VNode -> Range -> D.Verif VerifResult
verifExponent node range = error ""





