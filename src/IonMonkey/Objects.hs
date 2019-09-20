module IonMonkey.Objects ( Range
                         , lower
                         , upper
                         , rangeName
                         , newInputRange
                         , newResultRange
                         , operandWithRange
                         , verifyPossible
                         , verifyInRange
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

operandWithRange :: (D.MonadBoolector m) => String -> m D.Sort -> Range -> m D.Node
operandWithRange name sort range = do
  operand <- D.var' sort name
  D.slte operand (upper range) >>= D.assert
  D.sgte operand (lower range) >>= D.assert
  return operand

verifyInRange :: (D.MonadBoolector m) => D.Node -> Range -> m ()
verifyInRange node range = do
  D.sgt node (upper range) >>= D.assert
  D.slt node (lower range) >>= D.assert

verifyPossible :: (D.MonadBoolector m) => D.Node -> Range -> m ()
verifyPossible node range = do
  D.slte node (upper range) >>= D.assert
  D.sgte node (lower range) >>= D.assert



