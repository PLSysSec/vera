module IonMonkey.Objects ( Range
                         , VerifResult(..)
                         , lower
                         , upper
                         , rangeName
                         , signedInputRange
                         , unsignedInputRange
                         , signedResultRange
                         , unsignedResultRange
                         , operandWithRange
                         , verifySaneRange
                         , verifyUpperBound
                         , verifyLowerBound
                         ) where
import qualified Data.Map.Strict as M
import qualified DSL.DSL         as D
import           DSL.Typed       as T

-- | IonMonkey's range object
-- https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#119
data Range = Range {
      rangeName :: String
    , lower     :: T.VNode
    , upper     :: T.VNode
    }

signedInputRange :: String -> D.Verif Range
signedInputRange operandName = do
  let lowerName = operandName ++ "_lower"
      upperName = operandName ++ "_upper"
  lowerNode <- T.newInputVar T.Signed lowerName
  upperNode <- T.newInputVar T.Signed upperName
  T.cppLte lowerNode upperNode >>= T.vassert
  return $ Range operandName lowerNode upperNode

unsignedInputRange :: String -> D.Verif Range
unsignedInputRange operandName = do
  let lowerName = operandName ++ "_lower"
      upperName = operandName ++ "_upper"
  lowerNode <- T.newInputVar T.Unsigned lowerName
  upperNode <- T.newInputVar T.Unsigned upperName
  T.cppLte lowerNode upperNode >>= T.vassert
  return $ Range operandName lowerNode upperNode

signedResultRange :: String -> D.Verif Range
signedResultRange operandName = do
  let lowerName = operandName ++ "_lower"
      upperName = operandName ++ "_upper"
  lowerNode <- T.newResultVar T.Signed lowerName
  upperNode <- T.newResultVar T.Signed upperName
  return $ Range operandName lowerNode upperNode

unsignedResultRange :: String -> D.Verif Range
unsignedResultRange operandName = do
  let lowerName = operandName ++ "_lower"
      upperName = operandName ++ "_upper"
  lowerNode <- T.newResultVar T.Unsigned lowerName
  upperNode <- T.newResultVar T.Unsigned upperName
  return $ Range operandName lowerNode upperNode


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
                 | Broken { counterexample :: M.Map String Integer}
                 deriving (Eq, Ord, Show)

getResult :: D.SMTResult -> D.Verif VerifResult
getResult status = case status of
  D.SolverUnsat  -> return Verified
  D.SolverSat xs -> return $ Broken xs
  _              -> error "Solver error when verifying"

-- | Verify that the upper bound of a range is greater than the lower
-- Expects UNSAT
verifySaneRange :: Range -> D.Verif VerifResult
verifySaneRange resultRange = do
  D.push 1
  T.cppLt (upper resultRange) (lower resultRange) >>= T.vassert
  check <- D.runSolver
  D.pop 1
  getResult check

-- | Verify that a node is less than the upper bound
-- Expects UNSAT
verifyUpperBound :: T.VNode -> Range -> D.Verif VerifResult
verifyUpperBound node range = do
  D.push 1
  T.cppGt node (upper range) >>= T.vassert
  check <- D.runSolver
  D.pop 1
  getResult check

-- | Verify that a node is greater than the lower bound
-- Expects UNSAT
verifyLowerBound :: T.VNode -> Range -> D.Verif VerifResult
verifyLowerBound node range = do
  D.push 1
  T.cppLt node (lower range) >>= T.vassert
  check <- D.runSolver
  D.pop 1
  getResult check

