module IonMonkey.Objects ( Range
                         , lower
                         , upper
                         , rangeName
                         , newRange
                         , operandWithRange
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

newRange :: (D.MonadBoolector m) => String -> m D.Sort -> m Range
newRange operandName sort = do
  let lowerName = operandName ++ "_lower"
      upperName = operandName ++ "_upper"
  lowerNode <- D.var' sort lowerName
  upperNode <- D.var' sort upperName
  D.slte lowerNode upperNode >>= D.assert
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
  D.slte node (lower range) >>= D.assert




