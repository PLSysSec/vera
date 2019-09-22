module IonMonkey.Operations ( add
                            , sub
                            , and
                            , or
                            , xor
                            , not
                            , mul
                            , lsh
                            , rsh
                            , ursh
                            , lsh'
                            , rsh'
                            , ursh'
                            , abs
                            , min
                            , max
                            ) where
import           Control.Monad.State.Strict (liftIO)
import qualified DSL.DSL                    as D
import           IonMonkey.Objects
import           Prelude                    hiding (abs, and, max, min, not, or)

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#744
add :: (D.MonadBoolector m) => m Range
add = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#775
sub :: (D.MonadBoolector m) => m Range
sub = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#805
-- IonMonkey function only applies to i32s
-- Qutoes copied from the source comments
and :: (D.MonadBoolector m) => Range -> Range -> m Range
and lhs rhs = undefined
  -- -- MLFB: The updated range after the 'and' operation
  -- result <- newRange "result" D.i32

  -- -- If both numbers can be negative, result can be negative in the whole range
  -- zero <- D.i32c 0
  -- i32min <- D.i32min
  -- lhsNeg <- D.slt (lower lhs) zero
  -- rhsNeg <- D.slt (lower rhs) zero
  -- bothNeg <- D.and lhsNeg rhsNeg
  -- maxUpper <- D.smax (upper lhs) (upper rhs)
  -- D.condAssign bothNeg (lower result) i32min
  -- D.condAssign bothNeg (upper result) maxUpper

  -- -- Only one of both numbers can be negative.
  -- -- - result can't be negative
  -- -- - Upper bound is minimum of both upper range
  -- -- EXCEPT when upper bound of non negative number is max value,
  -- -- because negative value can return the whole max value.
  -- -- -1 & 5 = 5
  -- notBothNeg <- D.not bothNeg
  -- upperTemp <- D.smin (upper lhs) (upper rhs)
  -- D.condsAssign [notBothNeg, lhsNeg] upperTemp (upper rhs)
  -- D.condsAssign [notBothNeg, rhsNeg] upperTemp (upper lhs)
  -- D.condAssign notBothNeg (upper result) upperTemp
  -- return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#834
-- IonMonkey function only applies to i32s
or :: (D.MonadBoolector m) => Range -> Range -> m Range
or _lhs _rhs = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#893
xor :: (D.MonadBoolector m) => m Range
xor = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#955
not :: (D.MonadBoolector m) => Range -> m Range
not op = do
  result <- newResultRange "result" D.i32
  D.not (upper op) >>= D.assign (lower result)
  D.not (lower op) >>= D.assign (upper result)
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#960
mul :: (D.MonadBoolector m) => m Range
mul = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#999
lsh :: (D.MonadBoolector m) => m Range
lsh = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1016
rsh :: (D.MonadBoolector m) => Range -> D.Node -> m Range
rsh shiftee val = do
  -- Setup the shift
  thirtyOne <- D.i32c 31
  shift <- D.and val thirtyOne
  -- Make a new range whose low bound is shiftee_l >> shift, shiftee_h >> shift
  result <- newResultRange "result" D.i32
  D.safeSra (lower shiftee) shift >>= D.assign (lower result)
  D.safeSra (upper shiftee) shift >>= D.assign (upper result)
  return result

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





