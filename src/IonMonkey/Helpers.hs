module IonMonkey.Helpers ( isFiniteNonNegative
                         , isFiniteNegative
                         ) where
import qualified DSL.DSL           as D
import           IonMonkey.Objects

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#551
isFiniteNonNegative :: (D.MonadBoolector m) => Range -> m D.Node
isFiniteNonNegative range = do
  zero <- D.i32c 0
  D.sgte (lower range) zero

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#548
isFiniteNegative :: (D.MonadBoolector m) => Range -> m D.Node
isFiniteNegative range = do
  zero <- D.i32c 0
  D.slt (upper range) zero

