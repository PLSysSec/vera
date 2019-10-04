module IonMonkey.Objects ( Range
                         , VerifResult(..)
                         , lower
                         , upper
                         , hasInt32LowerBound
                         , hasInt32UpperBound
                         , canBeInfiniteOrNan
                         , canBeNegativeZero
                         , canHaveFractionalPart
                         , maxExponent
                         , rangeName
                         , signedInputRange
                         , unsignedInputRange
                         , signedResultRange
                         , unsignedResultRange
                         , operandWithRange
                         , verifyConsistent
                         , verifySaneRange
                         , verifyUpperBound
                         , verifyLowerBound
                         , verifyDefinedResult
                         ) where
import qualified Data.Map.Strict as M
import qualified DSL.DSL         as D
import           DSL.Typed       as T

-- | IonMonkey's range object
-- https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#119
data Range = Range {
      rangeName              :: String
    , lower                  :: T.VNode
    , upper                  :: T.VNode
    , hasInt32LowerBound_    :: Maybe T.VNode
    , hasInt32UpperBound_    :: Maybe T.VNode
    , canBeInfiniteOrNan_    :: Maybe T.VNode
    , canBeNegativeZero_     :: Maybe T.VNode
    , canHaveFractionalPart_ :: Maybe T.VNode
    , maxExponent_           :: Maybe T.VNode
    }

hasInt32LowerBound :: Range -> T.VNode
hasInt32LowerBound r = getField r hasInt32LowerBound_

hasInt32UpperBound :: Range -> T.VNode
hasInt32UpperBound r = getField r hasInt32UpperBound_

canBeInfiniteOrNan :: Range -> T.VNode
canBeInfiniteOrNan r = getField r canBeInfiniteOrNan_

canBeNegativeZero :: Range -> T.VNode
canBeNegativeZero r = getField r canBeNegativeZero_

canHaveFractionalPart :: Range -> T.VNode
canHaveFractionalPart r = getField r canHaveFractionalPart_

maxExponent :: Range -> T.VNode
maxExponent r = getField r maxExponent_

getField :: Range -> (Range -> Maybe T.VNode) -> T.VNode
getField r getter = case getter r of
                      Nothing -> error "Tried to access non-existent flag in range"
                      Just f  -> f

thirtyTwoBitRange :: String -> T.VNode -> T.VNode -> Range
thirtyTwoBitRange s l u = Range s l u Nothing Nothing Nothing Nothing Nothing Nothing

signedInputRange :: String -> D.Verif Range
signedInputRange operandName = do
  let lowerName = operandName ++ "_lower"
      upperName = operandName ++ "_upper"
  lowerNode <- T.newInputVar T.Signed lowerName
  upperNode <- T.newInputVar T.Signed upperName
  T.cppLte lowerNode upperNode >>= T.vassert
  return $ thirtyTwoBitRange operandName lowerNode upperNode

unsignedInputRange :: String -> D.Verif Range
unsignedInputRange operandName = do
  let lowerName = operandName ++ "_lower"
      upperName = operandName ++ "_upper"
  lowerNode <- T.newInputVar T.Unsigned lowerName
  upperNode <- T.newInputVar T.Unsigned upperName
  T.cppLte lowerNode upperNode >>= T.vassert
  return $ thirtyTwoBitRange operandName lowerNode upperNode

signedResultRange :: String -> D.Verif Range
signedResultRange operandName = do
  let lowerName = operandName ++ "_lower"
      upperName = operandName ++ "_upper"
  lowerNode <- T.newResultVar T.Signed lowerName
  upperNode <- T.newResultVar T.Signed upperName
  return $ thirtyTwoBitRange operandName lowerNode upperNode

unsignedResultRange :: String -> D.Verif Range
unsignedResultRange operandName = do
  let lowerName = operandName ++ "_lower"
      upperName = operandName ++ "_upper"
  lowerNode <- T.newResultVar T.Unsigned lowerName
  upperNode <- T.newResultVar T.Unsigned upperName
  return $ thirtyTwoBitRange operandName lowerNode upperNode

numResultRange :: String -> D.Verif Range
numResultRange operandName = do
  -- Names
  let lowerName = operandName ++ "_lower"
      upperName = operandName ++ "_upper"
  lowerNode <- T.newResultVar T.Unsigned lowerName
  upperNode <- T.newResultVar T.Unsigned upperName
  -- Flags
  let fractionFlagName = operandName ++ "_fractional"
      negZeroFlagName  = operandName ++ "_negativeZero"
      expName          = operandName ++ "_exp"
  fract <- T.newResultVar T.Bool fractionFlagName >>= return . Just
  negz <- T.newResultVar T.Bool negZeroFlagName >>= return . Just
  exp <- T.newResultVar T.Unsigned16 expName >>= return . Just
  return $ Range operandName lowerNode upperNode Nothing Nothing Nothing negz fract exp

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

-- Verification functions and datatypes

data VerifResult = Verified
                 | UnsatImpl
                 | OverlappingRange { counterexample :: M.Map String Integer }
                 | BadLowerBound { counterexample :: M.Map String Integer }
                 | BadUpperBound { counterexample :: M.Map String Integer }
                 | UndefRange { counterexample :: M.Map String Integer }
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
  D.push 1
  T.cppLt (upper resultRange) (lower resultRange) >>= T.vassert
  check <- D.runSolver
  D.pop 1
  return $ case check of
    D.SolverUnsat  -> Verified
    D.SolverSat xs -> OverlappingRange xs
    _              -> error "Error while verifying"

-- | Verify that a node is less than the upper bound
-- Expects UNSAT
verifyUpperBound :: T.VNode -> Range -> D.Verif VerifResult
verifyUpperBound node range = do
  D.push 1
  T.cppGt node (upper range) >>= T.vassert
  check <- D.runSolver
  D.pop 1
  return $ case check of
    D.SolverUnsat  -> Verified
    D.SolverSat xs -> BadUpperBound xs
    _              -> error "Error while verifying"

-- | Verify that a node is greater than the lower bound
-- Expects UNSAT
verifyLowerBound :: T.VNode -> Range -> D.Verif VerifResult
verifyLowerBound node range = do
  D.push 1
  T.cppLt node (lower range) >>= T.vassert
  check <- D.runSolver
  D.pop 1
  return $ case check of
    D.SolverUnsat  -> Verified
    D.SolverSat xs -> BadLowerBound xs
    _              -> error "Error while verifying"

verifyDefinedResult :: Range -> D.Verif VerifResult
verifyDefinedResult range = do
  D.push 1
  T.assertUndef (lower range)
  T.assertUndef (upper range)
  check <- D.runSolver
  D.pop 1
  return $ case check of
    D.SolverUnsat  -> Verified
    D.SolverSat xs -> UndefRange xs
    _              -> error "Error while verifying"
