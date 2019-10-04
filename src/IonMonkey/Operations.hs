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
import           Data.Maybe        (fromJust)
import qualified DSL.Typed         as T
import           IonMonkey.Helpers
import           IonMonkey.Objects
import           Prelude           hiding (abs, and, max, min, not, or)

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#744
add :: Range -> Range -> T.Verif Range
add left right = do

  extLeftLower <- T.cppCast (lower left) T.Signed64
  extRightLower <- T.cppCast (lower right) T.Signed64
  tmp_l <- T.cppAdd extLeftLower extRightLower

  -- We have to do this the same annoying way they do in case it introduces any
  -- weirdness. How annoying
  notLeftLower <- T.cppNot (hasInt32LowerBound left)
  notRightLower <- T.cppNot (hasInt32LowerBound right)
  hasNoLower <- T.cppOr notLeftLower notRightLower
  noLower <- noInt32LowerBound
  l <- T.cppCond hasNoLower noLower tmp_l

  extLeftUpper <- T.cppCast (upper left) T.Signed64
  extRightUpper <- T.cppCast (upper right) T.Signed64
  tmp_h <- T.cppAdd extLeftLower extRightLower

  notLeftUpper <- T.cppNot (hasInt32UpperBound left)
  notRightUpper <- T.cppNot (hasInt32UpperBound right)
  hasNoUpper <- T.cppOr notLeftUpper notRightUpper
  noUpper <- noInt32UpperBound
  h <- T.cppCond hasNoUpper noUpper tmp_h

  e <- do
    -- uint16_t e = Max(lhs->max_exponent_, rhs->max_exponent_);
    e1 <- T.cppMax (maxExponent left) (maxExponent right)

    -- if (e <= Range::MaxFiniteExponent) ++e;
    one <- T.num 1
    e2 <- T.cppAdd e1 one
    maxExp <- maxFiniteExponent
    underMax <- T.cppLte e1 maxExp
    e3 <- T.cppCond underMax e2 e1

    -- if (lhs->canBeInfiniteOrNaN() && rhs->canBeInfiniteOrNaN())
    -- e = includesInfiniteAndNan
    let leftInfNan  = canBeInfiniteOrNan left
        rightInfNan = canBeInfiniteOrNan right
    infNanCond <- T.cppOr leftInfNan rightInfNan
    --T.cppCond infNanCond e2 e3
    error ""

  error ""


-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#775
sub :: Range -> Range -> T.Verif Range
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

--   result <- newResultRange "result" D.i32
--   zero <- T.num 0
--   neg1 <- T.num -1
--   UINT32_MAX <- T.uintMax
--   INT32_MAX <- T.intMax
--   INT32_MIN <- T.intMin
--
--   lhsEq          <- T.cppEq (lower _lhs) (upper _lhs) -- lhs lower == lhs upper
--   lhsLowerEq0    <- T.cppEq (lower _lhs) zero         -- lhs lower == 0
--   lhsLowerEqNeg1 <- T.cppEq (lower _lhs) neg1         -- lhs lower == -1
--
--   lhsEqAndlhsLowerEq0 <- T.cppAnd lhsEq lhsLowerEq0
--   lhsEqAndlhsLowerEqNeg1 <- T.cppAnd lhsEq lhsLowerEqNeg1
--
--   rhsEq          <- T.cppEq (lower _rhs) (upper _rhs) -- rhs lower == rhs upper
--   rhsLowerEq0    <- T.cppEq (lower _rhs) zero         -- rhs lower == 0
--   rhsLowerEqNeg1 <- T.cppEq (lower _rhs) neg1         -- rhs lower == -1
--
--   -- lines 841-856
--
--
--   -- second part (
--
--   lhsLowerGte0   <- T.cppGte (lower _lhs) zero        -- lhs lower >= 0
--   rhsLowerGte0   <- T.cppGte (lower _rhs) zero        -- rhs lower >= 0
--   rhsAndRhsLowerGte0 <- T.cppAnd lhsLowerGte0 rhsLowerGte0
--
--   lower0 <- T.cppMax (lower _lhs) (lower rhs)
--   upper0 <- do t0 <- countLeadingZeroes32 (upper _lhs)
--                t1 <- countLeadingZeroes32 (upper _rhs)
--                T.cppMin t0 t1
--
--   lhsUpperLt0  <- T.cppLt (upper _lhs) zero        -- lhs upper < 0
--   rhsUpperLt0  <- T.cppLt (upper _rhs) zero        -- rhs upper < 0
--
--   lower1 <- do t0 <- T.cppNeg (lower lhs)
--                leadingOnes <- countLeadingZeroes32 t0 -- naming is confusing [sic]
--                t1 <- T.cppUshr UINT32_MAX leadingOnes
--                t2 <- T.cppNeg t1
--                T.cppMax INT32_MIN t2
--
--   lower2 <- do t0 <- T.cppNeg (lower rhs)
--                leadingOnes <- countLeadingZeroes32 t0 -- naming is confusing [sic]
--                t1 <- T.cppUshr UINT32_MAX leadingOnes
--                t2 <- T.cppNeg t1
--                t3 <- T.cppCond lhsUpperLt0 lower1 INT32_MIN
--                T.cppMax t3 t2
--
--   lhsLowerGte0AndRhsGte0    <- T.cppAnd lhsLowerGte0 rhsLowerGte0
--   lhsUpperLt0AndRhsUpperLt0 <- T.cppAnd lhsUpperLt0 rhsUpperLt0
--   lhsUpperLt0AndNotRhsUpperLt0 <- do t0 <- T.cppNot rhsUpperLt0
--                                      T.cppAnd lhsUpperLt0 t0
--
--   -- lines 868-889
--   lowerEnd <- cppCond (T.cppAnd lhsLowerGte0 rhsLowerGte0)
--               lower0
--               (cppCond rhsUpperLt0 lower2 (cppCond lhsUpperLt0 lower1 INT32_MIN))
--   upperEnd <- cppCond (T.cppAnd lhsLowerGte0 rhsLowerGte0)
--               upper0
--               (cppCond rhsUpperLt0 neg1 (cppCond lhsUpperLt0 neg1 INT32_MAX))


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
  i32min <- T.intMin
  i32max <- T.intMax

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





