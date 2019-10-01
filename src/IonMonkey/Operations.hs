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
import           IonMonkey.Helpers
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
and :: Range -> Range -> D.Verif Range
and left right = do

  result <- newResultRange "result" D.i32
  zero <- D.i32c 0

  -- Condition for choosing the return value
  leftNeg <- D.slt (lower left) zero
  rightNeg <- D.slt (lower right) zero
  neg <- D.and leftNeg rightNeg

  -- The upper and lower bounds in the true case
  trueLower <- D.i32min
  trueUpper <- D.smax (upper left) (upper right)

  -- The upper and lower bounds in the false case
  let falseLower = zero
  falseUpper <- do
    tmpUpper1 <- D.smin (upper left) (upper right)
    tmpUpper2 <- D.cond leftNeg (upper right) tmpUpper1
    D.cond rightNeg (upper left) tmpUpper2

  -- Assign the new upper and lower based on the condition from before
  D.cond neg trueLower falseLower >>= D.assign (lower result)
  D.cond neg trueUpper falseUpper >>= D.assign (upper result)
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#834
-- IonMonkey function only applies to i32s
or :: (D.MonadBoolector m) => Range -> Range -> m Range
or _lhs _rhs = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#893
xor :: (D.MonadBoolector m) => Range -> Range -> m Range
xor left right = undefined

  -- invertAfter <- D.false



  --  if (lhsUpper < 0) {
  --                    lhsLower = ~lhsLower;
  --                    lhsUpper = ~lhsUpper;
  --                    Swap(lhsLower, lhsUpper);
  --                    invertAfter = !invertAfter;
  --                  }
  --           if (rhsUpper < 0) {
  --                      rhsLower = ~rhsLower;
  --                      rhsUpper = ~rhsUpper;
  --                      Swap(rhsLower, rhsUpper);
  --                      invertAfter = !invertAfter;
  --                    })


-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#955
not :: Range -> D.Verif Range
not op = do
  result <- newResultRange "result" D.i32
  D.not (upper op) >>= D.assign (lower result)
  D.not (lower op) >>= D.assign (upper result)
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#960
mul :: (D.MonadBoolector m) => m Range
mul = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#999
lsh :: Range -> D.Node -> D.Verif Range
lsh shiftee val = do
  -- Setup the shift
  thirtyOne <- D.i32c 31
  one <- D.i32c 1
  shift <- D.and val thirtyOne

  -- Desired range
  shiftedLower <- D.safeSll (lower shiftee) shift
  shiftedUpper <- D.safeSll (upper shiftee) shift

  -- Compute the branch conditions
  doesntLoseBits <- do
    lowerDoesntLoseBits <- do
      tmp1 <- D.safeSll shiftedLower one
      tmp2 <- D.safeSrl tmp1 shift
      tmp3 <- D.safeSrl tmp2 one
      D.eq tmp3 (lower shiftee)

    upperDoesntLoseBits <- do
      tmp1 <- D.safeSll shiftedUpper one
      tmp2 <- D.safeSrl tmp1 shift
      tmp3 <- D.safeSrl tmp2 one
      D.eq tmp3 (upper shiftee)

    D.and lowerDoesntLoseBits upperDoesntLoseBits

  result <- newResultRange "result" D.i32

  -- fallback range
  i32min <- D.i32min
  i32max <- D.i32max

  D.cond doesntLoseBits shiftedLower i32min >>= D.assign (lower result)
  D.cond doesntLoseBits shiftedUpper i32max >>= D.assign (upper result)

  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1016
rsh :: Range -> D.Node -> D.Verif Range
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
ursh :: Range -> D.Node -> D.Verif Range
ursh lhs c = do
  -- Setup the shift
  thirtyOne <- D.i32c 31
  shift <- D.and c thirtyOne

  -- "If the value is always non-negative or always negative, we can simply
  -- compute the correct range by shifting."
  isNeg         <- isFiniteNegative lhs
  isNonNeg      <- isFiniteNonNegative lhs
  isNegOrNonNeg <- D.or isNeg isNonNeg

  trueLower  <- D.safeSrl (lower lhs) shift
  trueUpper  <- D.safeSrl (upper lhs) shift
  falseLower <- D.i32c 0
  falseUpper <- D.ui32max >>= \max -> D.safeSrl max shift

  result <- newResultRange "result" D.i32
  D.cond isNegOrNonNeg trueLower falseLower >>= D.assign (lower result)
  D.cond isNegOrNonNeg trueUpper falseUpper >>= D.assign (upper result)
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1042
lsh' :: Range -> Range -> D.Verif Range
lsh' _ _ = do
  -- Trivially correct
  result <- newResultRange "result" D.i32
  D.i32min >>= D.assign (lower result)
  D.i32max >>= D.assign (upper result)
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1048
rsh' :: Range -> Range -> D.Verif Range
rsh' shiftee shifter = do

  thirtyOne32 <- D.i32c 31
  zero <- D.i32c 0

  -- Cannonicalize shift range from 0-31
  cond <- do
    extShiftLower <- D.sext (lower shifter) 32
    extShiftUpper <- D.sext (upper shifter) 32
    sub <- D.sub extShiftUpper extShiftLower
    thirtyOne <- D.i64c 31
    D.sgte sub thirtyOne

  trueShiftLower <- D.i32c 0
  trueShiftUpper <- D.i32c 31

  tmpLower <- D.and thirtyOne32 (lower shifter)
  tmpUpper <- D.and thirtyOne32 (upper shifter)
  tmpCond <- D.sgt tmpLower tmpUpper
  falseShiftLower <- D.cond tmpCond zero        tmpLower
  falseShiftUpper <- D.cond tmpCond thirtyOne32 tmpUpper

  shiftLower <- D.cond cond trueShiftLower falseShiftLower
  shiftUpper <- D.cond cond trueShiftUpper falseShiftUpper

  -- Do the actual shifting
  min <- do
    shifteeNeg <- D.slt (lower shiftee) zero
    trueMin <- D.safeSra (lower shiftee) shiftLower
    falseMin <- D.safeSra (lower shiftee) shiftUpper
    D.cond shifteeNeg trueMin falseMin

  max <- do
    shifteeNeg <- D.sgte (upper shiftee) zero
    trueMax <- D.safeSra (upper shiftee) shiftLower
    falseMax <- D.safeSra (upper shiftee) shiftUpper
    D.cond shifteeNeg trueMax falseMax

  result <- newResultRange "result" D.i32
  D.assign (lower result) min
  D.assign (upper result) max
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1079
ursh' :: Range -> Range -> D.Verif Range
ursh' left _ = do
  isNonNeg <- isFiniteNonNegative left
  zero <- D.i32c 0
  uint32max <- D.ui32max

  result <- newResultRange "result" D.i32
  D.assign (lower result) zero
  D.cond isNonNeg (upper left) uint32max >>= D.assign (upper result)
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1089
abs :: (D.MonadBoolector m) => m Range
abs = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1104
min :: (D.MonadBoolector m) => m Range
min = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1123
max :: (D.MonadBoolector m) => m Range
max = undefined





