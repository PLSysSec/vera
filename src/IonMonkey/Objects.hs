module IonMonkey.Objects ( Range
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
import qualified DSL.DSL as D

-- IonMonkey's range object
-- https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#119
data Range = Range {
      rangeName :: String
    , lower     :: D.Node
    , upper     :: D.Node
    }

-- | We assume that an input range will have the invariant
-- that lower <= upper, since we assume inputs are working correctly
newInputRange :: (D.MonadBoolector m) => String -> m D.Sort -> m Range
newInputRange operandName sort = do
  let lowerName = operandName ++ "_lower"
      upperName = operandName ++ "_upper"
  lowerNode <- D.var' sort lowerName
  upperNode <- D.var' sort upperName
  D.slte lowerNode upperNode >>= D.assert
  return $ Range operandName lowerNode upperNode

-- | We do *not* assume that output ranges are working correctly.
-- That is an invariant that we will check
newResultRange :: (D.MonadBoolector m) => String -> m D.Sort -> m Range
newResultRange operandName sort = do
  let lowerName = operandName ++ "_lower"
      upperName = operandName ++ "_upper"
  lowerNode <- D.var' sort lowerName
  upperNode <- D.var' sort upperName
  return $ Range operandName lowerNode upperNode

-- | Make a new operand with name 'name' of sort 'sort' that is in the range
--'range'---ie is greater than the range's lower and less than the range's upper
operandWithRange :: (D.MonadBoolector m) => String -> m D.Sort -> Range -> m D.Node
operandWithRange name sort range = do
  operand <- D.var' sort name
  D.slte operand (upper range) >>= D.assert
  D.sgte operand (lower range) >>= D.assert
  return operand

-- | Verify that the upper bound of a range is greater than the lower
-- Expects UNSAT
-- TODO: make an informative datatype with a counterexample
verifySaneRange :: (D.MonadBoolector m) => Range -> m D.Status
verifySaneRange range = do
  D.push 1
  D.slt (upper range) (lower range) >>= D.assert
  check <- D.sat
  D.pop 1
  return check

-- | Verify that a node is less than the upper bound
-- Expects UNSAT
-- TODO same as above
verifyUpperBound :: (D.MonadBoolector m) => D.Node -> Range -> m D.Status
verifyUpperBound node range = do
  D.push 1
  D.sgt node (upper range) >>= D.assert
  check <- D.sat
  D.pop 1
  return check

-- | Verify that a node is greater than the lower bound
-- Expects UNSAT
-- TODO same as above
verifyLowerBound :: (D.MonadBoolector m) => D.Node -> Range -> m D.Status
verifyLowerBound node range = do
  D.push 1
  D.slt node (lower range) >>= D.assert
  check <- D.sat
  D.pop 1
  return check

