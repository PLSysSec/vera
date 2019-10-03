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
import qualified DSL.Typed                  as T
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

  result <- signedResultRange "result"
  zero <- T.num 0

  -- Condition for choosing the return value
  leftNeg <- T.cppLt (lower left) zero
  rightNeg <- T.cppLt (lower right) zero
  neg <- T.cppAnd leftNeg rightNeg

  -- The upper and lower bounds in the true case
  trueLower <- T.intMax
  trueUpper <- T.cppMax (upper left) (upper right)

  -- The upper and lower bounds in the false case
  let falseLower = zero
  falseUpper <- do
    tmpUpper1 <- T.cppMin (upper left) (upper right)
    tmpUpper2 <- T.cppCond leftNeg (upper right) tmpUpper1
    T.cppCond rightNeg (upper left) tmpUpper2

  -- Assign the new upper and lower based on the condition from before
  T.cppCond neg trueLower falseLower >>= T.vassign (lower result)
  T.cppCond neg trueUpper falseUpper >>= T.vassign (upper result)
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#834
-- IonMonkey function only applies to i32s
or :: (D.MonadBoolector m) => Range -> Range -> m Range
or _lhs _rhs = undefined

  -- result <- newResultRange "result" D.i32
  -- zero <- D.i32c 0
  -- neg1 <- D.i32c -1
  --
  -- -- lhs lower == lhs upper
  -- lhsEq <- D.eq (lower _lhs) (upper _lhs)
  -- lhsLowerEq0 <- D.eq (lower _lhs) zero
  -- lhsLowerEqNeg1 <- D.eq (lower _lhs) neg1
  --
  -- -- rhs lower == rhs upper
  -- rhsEq <- D.eq (lower _rhs) (upper _rhs)
  -- rhsLowerEq0 <- D.eq (lower _rhs) zero
  -- rhsLowerEqNeg1 <- D.eq (lower _rhs) neg1

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
  result <- signedResultRange "result"
  T.cppNot (upper op) >>= T.vassign (lower result)
  T.cppNot (lower op) >>= T.vassign (upper result)
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#960
mul :: (D.MonadBoolector m) => m Range
mul = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#999
lsh :: Range -> T.VNode -> D.Verif Range
lsh shiftee val = do
  -- Setup the shift
  thirtyOne <- T.num 31
  one <- T.num 1
  shift <- T.cppAnd val thirtyOne

  -- Desired range
  shiftedLower <- T.cppShiftLeft (lower shiftee) shift
  shiftedUpper <- T.cppShiftLeft (upper shiftee) shift

  -- Compute the branch conditions
  doesntLoseBits <- do
    lowerDoesntLoseBits <- do
      tmp1 <- T.cppShiftLeft shiftedLower one
      tmp2 <- T.cppShiftRight tmp1 shift
      tmp3 <- T.cppShiftRight tmp2 one
      T.cppEq tmp3 (lower shiftee)

    upperDoesntLoseBits <- do
      tmp1 <- T.cppShiftLeft shiftedUpper one
      tmp2 <- T.cppShiftRight tmp1 shift
      tmp3 <- T.cppShiftRight tmp2 one
      T.cppEq tmp3 (upper shiftee)

    T.cppAnd lowerDoesntLoseBits upperDoesntLoseBits

  result <- signedResultRange "result"

  -- fallback range
  i32min <- T.intMax
  i32max <- T.intMin

  T.cppCond doesntLoseBits shiftedLower i32min >>= T.vassign (lower result)
  T.cppCond doesntLoseBits shiftedUpper i32max >>= T.vassign (upper result)

  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1016
rsh :: Range -> T.VNode -> D.Verif Range
rsh shiftee val = do
  -- Setup the shift
  thirtyOne <- T.num 31
  shift <- T.cppAnd val thirtyOne
  -- Make a new range whose low bound is shiftee_l >> shift, shiftee_h >> shift
  result <- signedResultRange "result"
  T.cppShiftRight (lower shiftee) shift >>= T.vassign (lower result)
  T.cppShiftRight (upper shiftee) shift >>= T.vassign (upper result)
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1023
ursh :: Range -> T.VNode -> D.Verif Range
ursh lhs c = do
  --  int32_t shift = c & 0x1f;
  thirtyOne <- T.num 31
  shift <- T.cppAnd c thirtyOne

  -- "If the value is always non-negative or always negative, we can simply
  -- compute the correct range by shifting."
  isNeg         <- isFiniteNegative lhs
  isNonNeg      <- isFiniteNonNegative lhs
  isNegOrNonNeg <- T.cppOr isNeg isNonNeg

  trueLower  <- T.cppShiftRight (lower lhs) shift
  trueUpper  <- T.cppShiftRight (upper lhs) shift
  falseLower <- T.unum 0
  falseUpper <- T.uintMax >>= \max -> T.cppShiftRight max shift

  result <- unsignedResultRange "result"
  T.cppCond isNegOrNonNeg trueLower falseLower >>= T.vassign (lower result)
  T.cppCond isNegOrNonNeg trueUpper falseUpper >>= T.vassign (upper result)
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1042
lsh' :: Range -> Range -> D.Verif Range
lsh' _ _ = do
  -- Trivially correct
  result <- signedResultRange "result"
  T.intMin >>= T.vassign (lower result)
  T.intMax >>= T.vassign (upper result)
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1048
rsh' :: Range -> Range -> D.Verif Range
rsh' shiftee shifter = error "Not yet ported"

  -- thirtyOne <- T.num 31
  -- zero <- T.num 0

  -- -- Cannonicalize shift range from 0-31
  -- cond <- do
  --   extShiftLower <- D.sext (lower shifter) 32
  --   extShiftUpper <- D.sext (upper shifter) 32
  --   sub <- D.sub extShiftUpper extShiftLower
  --   thirtyOne <- D.i64c 31
  --   D.sgte sub thirtyOne

  -- trueShiftLower <- D.i32c 0
  -- trueShiftUpper <- D.i32c 31

  -- tmpLower <- D.and thirtyOne32 (lower shifter)
  -- tmpUpper <- D.and thirtyOne32 (upper shifter)
  -- tmpCond <- D.sgt tmpLower tmpUpper
  -- falseShiftLower <- D.cond tmpCond zero        tmpLower
  -- falseShiftUpper <- D.cond tmpCond thirtyOne32 tmpUpper

  -- shiftLower <- D.cond cond trueShiftLower falseShiftLower
  -- shiftUpper <- D.cond cond trueShiftUpper falseShiftUpper

  -- -- Do the actual shifting
  -- min <- do
  --   shifteeNeg <- D.slt (lower shiftee) zero
  --   trueMin <- D.safeSra (lower shiftee) shiftLower
  --   falseMin <- D.safeSra (lower shiftee) shiftUpper
  --   D.cond shifteeNeg trueMin falseMin

  -- max <- do
  --   shifteeNeg <- D.sgte (upper shiftee) zero
  --   trueMax <- D.safeSra (upper shiftee) shiftLower
  --   falseMax <- D.safeSra (upper shiftee) shiftUpper
  --   D.cond shifteeNeg trueMax falseMax

  -- result <- newResultRange "result" D.i32
  -- D.assign (lower result) min
  -- D.assign (upper result) max
  -- return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1079
ursh' :: Range -> Range -> D.Verif Range
ursh' left _ = do
  isNonNeg <- isFiniteNonNegative left
  zero <- T.unum 0
  uint32max <- T.uintMax

  result <- unsignedResultRange "result"
  T.vassign (lower result) zero
  T.cppCond isNonNeg (upper left) uint32max >>= T.vassign (upper result)
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





