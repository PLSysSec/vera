module IonMonkey.Helpers ( isFiniteNonNegative
                         , isFiniteNegative
                         ) where
import qualified DSL.DSL           as D
import           IonMonkey.Objects

isFiniteNonNegative :: (D.MonadBoolector m) => Range -> m D.Node
isFiniteNonNegative = undefined

isFiniteNegative :: (D.MonadBoolector m) => Range -> m D.Node
isFiniteNegative = undefined

