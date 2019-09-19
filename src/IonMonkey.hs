module IonMonkey ( Range
                 , lower
                 , upper
                 , newRange
                 ) where
import qualified DSL as D

-- IonMonkey's range object
-- URL
data Range = Range {
      name  :: String
    , lower :: D.Node
    , upper :: D.Node
    }

newRange :: (D.MonadBoolector m) => String -> m D.Sort -> m Range
newRange operandName sort = do
  let lowerName = operandName ++ "_lower"
      upperName = operandName ++ "_upper"
  lowerNode <- D.var' sort lowerName
  upperNode <- D.var' sort upperName
  return $ Range operandName lowerNode upperNode




