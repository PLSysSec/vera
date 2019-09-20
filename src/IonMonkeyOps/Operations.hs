module IonMonkeyOps.Operations (and) where
import           Control.Monad.State.Strict    (liftIO)
import qualified DSL.DSL                       as D
import           IonMonkeyOps.IonMonkeyObjects
import           Prelude                       hiding (and)

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#744
add :: (D.MonadBoolector m) => m Range
add = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#775
sub :: (D.MonadBoolector m) => m Range
sub = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#805
-- Qutoes copied from the source comments
and :: (D.MonadBoolector m) => Range -> Range -> m Range
and lhs rhs = do
  -- MLFB: The updated range after the 'and' operation
  result <- newRange "result" D.i32

  -- If both numbers can be negative, result can be negative in the whole range
  zero <- D.i32c 0
  i32min <- D.i32min
  lhsNeg <- D.slt (lower lhs) zero
  rhsNeg <- D.slt (lower rhs) zero
  bothNeg <- D.and lhsNeg rhsNeg
  maxUpper <- D.smax (upper lhs) (upper rhs)
  D.condAssign bothNeg (lower result) i32min
  D.condAssign bothNeg (upper result) maxUpper

  -- Only one of both numbers can be negative.
  -- - result can't be negative
  -- - Upper bound is minimum of both upper range
  -- EXCEPT when upper bound of non negative number is max value,
  -- because negative value can return the whole max value.
  -- -1 & 5 = 5
  notBothNeg <- D.not bothNeg
  upperTemp <- D.smin (upper lhs) (upper rhs)
  D.condsAssign [notBothNeg, lhsNeg] (upper result) (upper rhs)
  D.condsAssign [notBothNeg, rhsNeg] (upper result) (upper lhs)
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#834
or :: (D.MonadBoolector m) => m Range
or = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#893
xor :: (D.MonadBoolector m) => m Range
xor = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#955
not :: (D.MonadBoolector m) => m Range
not = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#960
mul :: (D.MonadBoolector m) => m Range
mul = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#999
lsh :: (D.MonadBoolector m) => m Range
lsh = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1016
rsh :: (D.MonadBoolector m) => m Range
rsh = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1023
ursh :: (D.MonadBoolector m) => m Range
ursh = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1042
lsh' :: (D.MonadBoolector m) => m Range
lsh' = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1048
rsh' :: (D.MonadBoolector m) => m Range
rsh' = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1079
ursh' :: (D.MonadBoolector m) => m Range
ursh' = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1089
abs :: (D.MonadBoolector m) => m Range
abs = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1104
min :: (D.MonadBoolector m) => m Range
min = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1123
max :: (D.MonadBoolector m) => m Range
max = undefined





