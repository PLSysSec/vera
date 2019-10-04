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
import qualified DSL.Typed         as T
import           IonMonkey.Helpers
import           IonMonkey.Objects
import           Prelude           hiding (abs, and, max, min, not, or)

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#744
add = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#775
sub = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#805
-- IonMonkey function only applies to i32s
and :: Range -> Range -> T.Verif Range
and left right = do

  result <- signedResultRange "result"
  zero <- T.num 0

  -- Condition for choosing the return value
  leftNeg <- T.cppLt (lower left) zero
  rightNeg <- T.cppLt (lower right) zero
  neg <- T.cppAnd leftNeg rightNeg

  -- The upper and lower bounds in the true case
  trueLower <- T.intMin
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
not :: Range -> T.Verif Range
not op = do
  result <- signedResultRange "result"
  T.cppNot (upper op) >>= T.vassign (lower result)
  T.cppNot (lower op) >>= T.vassign (upper result)
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#960
mul = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#999
lsh :: Range -> T.VNode -> T.Verif Range
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
rsh :: Range -> T.VNode -> T.Verif Range
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
ursh :: Range -> T.VNode -> T.Verif Range
ursh lhs c = do
  --  int32_t shift = c & 0x1f;
  thirtyOne <- T.num 31
  shift <- T.cppAnd c thirtyOne

  -- "If the value is always non-negative or always negative, we can simply
  -- compute the correct range by shifting."
  isNeg         <- isFiniteNegative lhs
  isNonNeg      <- isFiniteNonNegative lhs
  isNegOrNonNeg <- T.cppOr isNeg isNonNeg

  trueLower  <- T.cppShiftRight (lower lhs) shift >>= \n -> T.cppCast n T.Unsigned
  trueUpper  <- T.cppShiftRight (upper lhs) shift >>= \n -> T.cppCast n T.Unsigned
  falseLower <- T.unum 0
  falseUpper <- T.uintMax >>= \m -> T.cppShiftRight m shift

  result <- unsignedResultRange "result"
  T.cppCond isNegOrNonNeg trueLower falseLower >>= T.vassign (lower result)
  T.cppCond isNegOrNonNeg trueUpper falseUpper >>= T.vassign (upper result)
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1042
lsh' :: Range -> Range -> T.Verif Range
lsh' _ _ = do
  -- Trivially correct
  result <- signedResultRange "result"
  T.intMin >>= T.vassign (lower result)
  T.intMax >>= T.vassign (upper result)
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1048
rsh' :: Range -> Range -> T.Verif Range
rsh' shiftee shifter = do

  thirtyOne <- T.num 31
  zero <- T.num 0

  -- Cannonicalize shift range from 0-31
  cond <- do
    extShiftLower <- T.cppCast (lower shifter) T.Signed64
    extShiftUpper <- T.cppCast (upper shifter) T.Signed64
    sub <- T.cppSub extShiftUpper extShiftLower
    thirtyOne <- T.num64 31
    T.cppGte sub thirtyOne

  trueShiftLower <- T.num 0
  trueShiftUpper <- T.num 31

  tmpLower <- T.cppAnd thirtyOne (lower shifter)
  tmpUpper <- T.cppAnd thirtyOne (upper shifter)
  tmpCond <- T.cppGt tmpLower tmpUpper
  falseShiftLower <- T.cppCond tmpCond zero      tmpLower
  falseShiftUpper <- T.cppCond tmpCond thirtyOne tmpUpper

  shiftLower <- T.cppCond cond trueShiftLower falseShiftLower
  shiftUpper <- T.cppCond cond trueShiftUpper falseShiftUpper

  -- Do the actual shifting
  min <- do
    shifteeNeg <- T.cppLt (lower shiftee) zero
    trueMin <- T.cppShiftRight (lower shiftee) shiftLower
    falseMin <- T.cppShiftRight (lower shiftee) shiftUpper
    T.cppCond shifteeNeg trueMin falseMin

  max <- do
    shifteeNeg <- T.cppGte (upper shiftee) zero
    trueMax <- T.cppShiftRight (upper shiftee) shiftLower
    falseMax <- T.cppShiftRight (upper shiftee) shiftUpper
    T.cppCond shifteeNeg trueMax falseMax

  result <- signedResultRange "result"
  T.vassign (lower result) min
  T.vassign (upper result) max
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1079
ursh' :: Range -> Range -> T.Verif Range
ursh' left _ = do
  -- Setup the shift
  isNonNeg <- isFiniteNonNegative left
  zero <- T.unum 0
  uint32max <- T.uintMax

  result <- unsignedResultRange "result"
  -- 0, lhs->isFiniteNonNegative() ? lhs->upper() : UINT32_MAX
  T.vassign (lower result) zero

  castUpper <- T.cppCast (upper left) T.Unsigned
  T.cppCond isNonNeg castUpper uint32max >>= T.vassign (upper result)
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1089
abs = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1104
min = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1123
max = undefined





