module IonMonkey.Objects ( Range
                         , VerifResult(..)
                         -- * Getters for fields of the range
                         , lower
                         , upper
                         , hasInt32LowerBound
                         , hasInt32UpperBound
                         , canBeInfiniteOrNan
                         , canBeNegativeZero
                         , canHaveFractionalPart
                         , maxExponent
                         , rangeName
                         -- * Making ranges and operands
                         , inputRange
                         , resultRange
                         , operandWithRange
                         -- * Verifying properties of ranges
                         , verifyConsistent
                         , verifySaneRange
                         , verifyUpperBound
                         , verifyLowerBound
                         , verifyDefinedResult
                         , verifyInfNan
                         , verifyNegZero
                         ) where
import           Control.Monad   (unless, when)
import           Data.List       (intersperse, isInfixOf)
import qualified Data.Map.Strict as M
import           Data.Maybe      (catMaybes)
import qualified DSL.DSL         as D
import           DSL.Typed       as T
import qualified DSL.Z3Wrapper   as D

-- | IonMonkey's range object
-- https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#119
data Range = Range {
      rangeName             :: String
    , lower                 :: T.VNode
    , upper                 :: T.VNode
    , hasInt32LowerBound    :: T.VNode
    , hasInt32UpperBound    :: T.VNode
    , canBeInfiniteOrNan    :: T.VNode
    , canBeNegativeZero     :: T.VNode
    , canHaveFractionalPart :: T.VNode
    , maxExponent           :: T.VNode
    }

inputRange :: Type -> String -> D.Verif Range
inputRange ty operandName = do

  -- Make and setup the int32 bounds
  let lowerName = operandName ++ "_lower"
      upperName = operandName ++ "_upper"
      -- If it's not a 32-bit input range, this should be irrelevant
      rangeType = if is32Bits ty then ty else T.Signed
  lowerNode <- T.newInputVar rangeType lowerName
  upperNode <- T.newInputVar rangeType upperName
  T.cppLte lowerNode upperNode >>= T.vassert

  -- Make the flags
  let hasLowerBoundName = operandName ++ "_hasLowerBound"
      hasUpperBoundName = operandName ++ "_hasUpperBound"
      infOrNanName      = operandName ++ "_infOrNan"
      negZeroName       = operandName ++ "_negZero"
      fractPartName     = operandName ++ "_hasFract"
      expName           = operandName ++ "_exp"
  hasLowerBound <- T.newInputVar T.Bool hasLowerBoundName
  hasUpperBound <- T.newInputVar T.Bool hasUpperBoundName
  infOrNan      <- T.newInputVar T.Bool infOrNanName
  negZero       <- T.newInputVar T.Bool negZeroName
  fractPart     <- T.newInputVar T.Bool fractPartName
  exp           <- T.newInputVar T.Unsigned16 expName

  -- Set the flags according to the type: if its an int, all are false,
  -- otherwise they could be anything
  when (is32Bits ty) $ do
    t <- T.true
    f <- T.false
    T.vassign hasLowerBound t
    T.vassign hasUpperBound t
    T.vassign infOrNan f
    T.vassign negZero f
    T.vassign fractPart f
    T.unum16 0 >>= T.vassign exp

  return $ Range operandName lowerNode upperNode hasLowerBound hasUpperBound infOrNan negZero fractPart exp


resultRange :: Type -> String -> D.Verif Range
resultRange ty operandName = do

  -- Make and setup the int32 bounds
  let lowerName = operandName ++ "_lower"
      upperName = operandName ++ "_upper"
      -- If it's not a 32-bit input range, this should be irrelevant
      rangeType = if is32Bits ty then ty else T.Signed
  lowerNode <- T.newResultVar rangeType lowerName
  upperNode <- T.newResultVar rangeType upperName

  -- Make the flags
  let hasLowerBoundName = operandName ++ "_hasLowerBound"
      hasUpperBoundName = operandName ++ "_hasUpperBound"
      infOrNanName      = operandName ++ "_infOrNan"
      negZeroName       = operandName ++ "_negZero"
      fractPartName     = operandName ++ "_hasFract"
      expName           = operandName ++ "_exp"
  hasLowerBound <- T.newResultVar T.Bool hasLowerBoundName
  hasUpperBound <- T.newResultVar T.Bool hasUpperBoundName
  infOrNan      <- T.newResultVar T.Bool infOrNanName
  negZero       <- T.newResultVar T.Bool negZeroName
  fractPart     <- T.newResultVar T.Bool fractPartName
  exp           <- T.newResultVar T.Unsigned16 expName

  return $ Range operandName lowerNode upperNode hasLowerBound hasUpperBound infOrNan negZero fractPart exp

-- | Make a new operand with name 'name' of sort 'sort' that is in the range
--'range'---ie is greater than the range's lower and less than the range's upper
operandWithRange :: String -> Type -> Range -> D.Verif T.VNode
operandWithRange name ty range = do
  op <- newInputVar ty name
  if isDouble ty
  then do
    -- If it can be inf or nan, the range must say so
    opIsInf <- T.isInf op
    opIsNan <- T.isNan op
    isInfOrNan <- T.cppOr opIsInf opIsNan
    T.vassign isInfOrNan (canBeInfiniteOrNan range)
    -- If it can be negative zero the range should say so
    isNeg <- T.isNeg op
    isZero <- T.isZero op
    isNegZero <- T.cppOr isNeg isZero
    T.vassign isNegZero (canBeNegativeZero range)
    -- If it can have a fractional part the range should say so

    -- If it can be outside of a standard int range, the flag should indicate so
    fpJsMax <- T.fpnum 2147483647
    fpJsMin <- T.fpnum (-2147483648)
    isTooBig <- T.cppGt op fpJsMax
    isTooSmall <- T.cppLt op fpJsMin
    f <- T.false
    let hasLower = hasInt32LowerBound range
        hasUpper = hasInt32UpperBound range
    hasLower' <- T.cppCond isTooSmall f hasLower
    hasUpper' <- T.cppCond isTooBig f hasUpper
    T.vassign hasLower hasLower'
    T.vassign hasUpper hasUpper'
  else do
    T.cppLte op (upper range) >>= T.vassert
    T.cppGte op (lower range) >>= T.vassert
  return op

--
-- Verification functions and datatypes
--

data VerifResult = Verified
                 | UnsatImpl
                 | OverlappingRange { counterexample :: M.Map String Float }
                 | BadLowerBound { counterexample :: M.Map String Float }
                 | BadUpperBound { counterexample :: M.Map String Float }
                 | UndefRange { counterexample :: M.Map String Float }
                 | BadNans { counterexample :: M.Map String Float }
                 | BadNegZ { counterexample :: M.Map String Float }
                 deriving (Eq, Ord)

instance Show VerifResult where
    show (OverlappingRange ce) = "Upper and lower of result range may overlap\n:" ++
                                 prettyCounterexampleInts ce
    show (BadLowerBound ce)    = "Example operation can be outside of lower boud\n:" ++
                                 prettyCounterexampleInts ce
    show (BadUpperBound ce)    = "Example operation can be outside of upper bound\n" ++
                                 prettyCounterexampleInts ce
    show (UndefRange ce)       = "Example operation may introduce undefined behavior\n" ++
                                 prettyCounterexampleInts ce
    show Verified              = "Verified!"
    show UnsatImpl             = "Verification failed (e.g., due to a timeout)"
    show ce                    = show $ counterexample ce

getIntList :: M.Map String Float -> [String]
getIntList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "_exp" `isInfixOf` str -> Nothing
                         _ | "_has" `isInfixOf` str -> Nothing
                         _ | "_negZero" `isInfixOf` str -> Nothing
                         _ | "infOrNan" `isInfixOf` str -> Nothing
                         _ -> Just $ unwords [str, ":", show $ round fl]
                     ) $ M.toList fls

prettyCounterexampleInts :: M.Map String Float
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
  -- It's nan or inf....
  T.cppOr nodeIsInf nodeIsNan >>= T.vassert
  --- .... but the nan or inf flag isnt set
  T.cppNeg (canBeInfiniteOrNan range) >>= T.vassert
  check1 <- D.runSolver
  D.pop
  D.push
  -- It's not nan or inf....
  T.cppOr nodeIsInf nodeIsNan >>= T.cppNeg >>= T.vassert
  -- ... but the nan or inf flag is set
  T.vassert $ canBeInfiniteOrNan range
  check2 <- D.runSolver
  D.pop
  return $ case check1 of
    D.SolverSat xs -> BadNans xs
    D.SolverFailed -> error "Error while verifying"
    D.SolverUnsat -> case check2 of
                       D.SolverUnsat  -> Verified
                       D.SolverSat xs -> BadNans xs
                       _              -> error "Error while verifying"

-- | IsFract flag set for a non-fract value?
-- | IsFract flag unset for a fact value?
veriftFract :: T.VNode -> Range -> D.Verif VerifResult
veriftFract node range = error ""

verifInt32Bounds :: T.VNode -> Range -> D.Verif VerifResult
verifInt32Bounds node range = error ""

verifyNegZero :: T.VNode -> Range -> D.Verif VerifResult
verifyNegZero node range = do
  isZero <- T.isZero node
  isNeg <- T.isNeg node
  D.push
  -- It's negative zero...
  T.cppAnd isZero isNeg >>= T.vassert
  -- ... but the nz flag is not set
  T.cppNeg (canBeNegativeZero range) >>= T.vassert
  check1 <- T.runSolver
  D.pop
  D.push
  -- It's not negative zero...
  T.cppAnd isZero isNeg >>= T.cppNeg >>= T.vassert
  -- but the nz flag is set
  T.vassert $ canBeNegativeZero range
  check2 <- T.runSolver
  D.pop
  return $ case check1 of
    D.SolverSat xs -> BadNegZ xs
    D.SolverFailed -> error "Error while verifying"
    D.SolverUnsat -> case check2 of
                       D.SolverUnsat  -> Verified
                       D.SolverSat xs -> BadNegZ xs
                       _              -> error "Error while verifying"

verifExponent :: T.VNode -> Range -> D.Verif VerifResult
verifExponent node range = error ""




