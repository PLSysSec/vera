module IonMonkey.Objects ( Range
                         , RangeResult(..)
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

data RangeResult = RangeVerified
                 | RangeBroken { inputsLower  :: [Integer]
                               , inputsUpper  :: [Integer]
                               , counterLower :: Integer
                               , counterUpper :: Integer
                               }
                 deriving (Eq, Ord, Show)

data BoundsResult = BoundsVerified
                  | BoundsBroken { counterResult :: Integer
                                 , counterBound  :: Integer
                                 }

-- | Verify that the upper bound of a range is greater than the lower
-- Expects UNSAT
-- TODO: make an informative datatype with a counterexample
verifySaneRange :: (D.MonadBoolector m) => [Range] -> Range -> m RangeResult
verifySaneRange inputRanges resultRange = do
  D.push 1
  D.slt (upper resultRange) (lower resultRange) >>= D.assert
  check <- D.sat
  D.pop 1
  case check of
    D.Unsat -> return RangeVerified
    D.Sat -> do
      inputsLowerAssign <- mapM (D.signedBvAssignment . lower) inputRanges
      inputsUpperAssign <- mapM (D.signedBvAssignment . upper) inputRanges
      lowerAssign <- D.signedBvAssignment (lower resultRange)
      upperAssign <- D.signedBvAssignment (upper resultRange)
      return $ RangeBroken inputsLowerAssign inputsUpperAssign lowerAssign upperAssign
    e -> error $ unwords ["Solver error when verifying the range:", show e]

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

