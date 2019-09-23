module IonMonkey.Objects ( Range
                         , VerifResult(..)
                         , lower
                         , upper
                         , rangeName
                         , newInputRange
                         , newResultRange
                         , operandWithRange
                         , verifySaneRange
                         , verifyUpperBound
                         , verifyLowerBound
                         ) where
import qualified Data.Map.Strict as M
import qualified DSL.DSL         as D

-- | IonMonkey's range object
-- https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#119
data Range = Range {
      rangeName :: String
    , lower     :: D.Node
    , upper     :: D.Node
    }

-- | We assume that an input range will have the invariant
-- that lower <= upper, since we assume inputs are working correctly
newInputRange :: String -> D.Verif D.Sort -> D.Verif Range
newInputRange operandName sort = do
  let lowerName = operandName ++ "_lower"
      upperName = operandName ++ "_upper"
  lowerNode <- D.var' sort lowerName
  upperNode <- D.var' sort upperName
  D.slte lowerNode upperNode >>= D.assert
  return $ Range operandName lowerNode upperNode

-- | We do *not* assume that output ranges are working correctly.
-- That is an invariant that we will check
newResultRange :: String -> D.Verif D.Sort -> D.Verif Range
newResultRange operandName sort = do
  let lowerName = operandName ++ "_lower"
      upperName = operandName ++ "_upper"
  lowerNode <- D.var' sort lowerName
  upperNode <- D.var' sort upperName
  return $ Range operandName lowerNode upperNode

-- | Make a new operand with name 'name' of sort 'sort' that is in the range
--'range'---ie is greater than the range's lower and less than the range's upper
operandWithRange :: String -> D.Verif D.Sort -> Range -> D.Verif D.Node
operandWithRange name sort range = do
  operand <- D.var' sort name
  D.slte operand (upper range) >>= D.assert
  D.sgte operand (lower range) >>= D.assert
  return operand

-- Verification functions and datatypes

data VerifResult = Verified
                 | Broken { counterexample :: M.Map String Integer}
                 deriving (Eq, Ord, Show)

getResult :: D.Status -> D.Verif VerifResult
getResult status = case status of
  D.Unsat -> return Verified
  D.Sat -> do
    allVars <- D.getVars
    assigns <- mapM D.signedBvAssignment allVars
    return $ Broken assigns
  e -> error $ unwords $ ["Solver error when verifying", show e]

-- | Verify that the upper bound of a range is greater than the lower
-- Expects UNSAT
-- TODO: make an informative datatype with a counterexample
verifySaneRange :: Range -> D.Verif VerifResult
verifySaneRange resultRange = do
  D.push 1
  D.slt (upper resultRange) (lower resultRange) >>= D.assert
  check <- D.sat
  D.pop 1
  getResult check

-- | Verify that a node is less than the upper bound
-- Expects UNSAT
-- TODO same as above
verifyUpperBound :: D.Node -> Range -> D.Verif VerifResult
verifyUpperBound node range = do
  D.push 1
  D.sgt node (upper range) >>= D.assert
  check <- D.sat
  D.pop 1
  getResult check

-- | Verify that a node is greater than the lower bound
-- Expects UNSAT
-- TODO same as above
verifyLowerBound :: D.Node -> Range -> D.Verif VerifResult
verifyLowerBound node range = do
  D.push 1
  D.slt node (lower range) >>= D.assert
  check <- D.sat
  D.pop 1
  getResult check

