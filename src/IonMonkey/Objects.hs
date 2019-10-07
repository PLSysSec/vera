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
                         ) where
import           Control.Monad   (unless, when)
import qualified Data.Map.Strict as M
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
  op <- case ty of
          T.Signed   -> T.int32 name
          T.Unsigned -> T.uint32 name
          _          -> error "we will fix this"
  T.cppLte op (upper range) >>= T.vassert
  T.cppGte op (lower range) >>= T.vassert
  return op

--
-- Verification functions and datatypes
--

data VerifResult = Verified
                 | UnsatImpl
                 | OverlappingRange { counterexample :: String }
                 | BadLowerBound { counterexample :: String }
                 | BadUpperBound { counterexample :: String }
                 | UndefRange { counterexample :: String }
                 deriving (Eq, Ord, Show)

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
verifyInfNan node range = error ""

-- | IsFract flag set for a non-fract value?
-- | IsFract flag unset for a fact value?
veriftFract :: T.VNode -> Range -> D.Verif VerifResult
veriftFract node range = error ""

verifInt32Bounds :: T.VNode -> Range -> D.Verif VerifResult
verifInt32Bounds node range = error ""

verifNegZero :: T.VNode -> Range -> D.Verif VerifResult
verifNegZero node range = error ""

verifExponent :: T.VNode -> Range -> D.Verif VerifResult
verifExponent node range = error ""




