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
    includesInfFlag <- includesInfinityAndNan
    T.cppCond infNanCond includesInfFlag e3

  fractFlag <- T.cppOr (canHaveFractionalPart left) (canHaveFractionalPart right)
  negZeroFlag <- T.cppOr (canBeNegativeZero left) (canBeNegativeZero right)

  result <- resultRange T.Double "result"
  T.vassign (lower result) l
  T.vassign (upper result) h
  T.vassign (canHaveFractionalPart result) fractFlag
  T.vassign (canBeNegativeZero result) negZeroFlag
  T.vassign (maxExponent result) e
  return result


-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#775
sub :: Range -> Range -> T.Verif Range
sub = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#805
-- IonMonkey function only applies to i32s
and :: Range -> Range -> T.Verif Range
and left right = do

  result <- resultRange T.Signed "result"
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
or :: Range -> Range -> T.Verif Range
or _lhs _rhs = do
  result <- resultRange T.Signed "result"
  zero <- T.num 0
  neg1 <- T.num (-1)
  uint32Max <- T.uintMax
  int32Max <- T.intMax
  int32Min <- T.intMin


  lhsEq          <- T.cppEq (lower _lhs) (upper _lhs) -- lhs lower == lhs upper
  lhsLowerEq0    <- T.cppEq (lower _lhs) zero         -- lhs lower == 0
  lhsLowerEqNeg1 <- T.cppEq (lower _lhs) neg1         -- lhs lower == -1

  lhsEqAndlhsLowerEq0 <- T.cppAnd lhsEq lhsLowerEq0
  lhsEqAndlhsLowerEqNeg1 <- T.cppAnd lhsEq lhsLowerEqNeg1

  rhsEq          <- T.cppEq (lower _rhs) (upper _rhs) -- rhs lower == rhs upper
  rhsLowerEq0    <- T.cppEq (lower _rhs) zero         -- rhs lower == 0
  rhsLowerEqNeg1 <- T.cppEq (lower _rhs) neg1         -- rhs lower == -1

  rhsEqAndrhsLowerEq0    <- T.cppAnd rhsEq rhsLowerEq0
  rhsEqAndrhsLowerEqNeg1 <- T.cppAnd rhsEq rhsLowerEqNeg1

  -- lines 841-856

  lhsLowerGte0   <- T.cppGte (lower _lhs) zero        -- lhs lower >= 0
  rhsLowerGte0   <- T.cppGte (lower _rhs) zero        -- rhs lower >= 0
  lhsAndrhsLowerGte0 <- T.cppAnd lhsLowerGte0 rhsLowerGte0

  -- 870,874
  lower0 <- T.cppMax (lower _lhs) (lower _rhs)
  upper0 <- do t0 <- countLeadingZeroes32 (upper _lhs)
               t1 <- countLeadingZeroes32 (upper _rhs)
               t2 <- T.cppMin t0 t1
               t3 <- T.cppShiftRight uint32Max t2
               T.cppCast t3 T.Signed

  lhsUpperLt0  <- T.cppLt (upper _lhs) zero        -- lhs upper < 0
  rhsUpperLt0  <- T.cppLt (upper _rhs) zero        -- rhs upper < 0

  -- 879
  lower1 <- do t0 <- T.cppNeg (lower _lhs)
               leadingOnes <- countLeadingZeroes32 t0 -- naming is confusing [sic]
               t1 <- T.cppShiftRight uint32Max leadingOnes
               t2 <- T.cppCast t1 T.Signed
               t3 <- T.cppNeg t2
               T.cppMax int32Min t3

  -- 884
  lower2 <- do t0 <- T.cppNeg (lower _rhs)
               leadingOnes <- countLeadingZeroes32 t0 -- naming is confusing [sic]
               t1 <- T.cppShiftRight uint32Max leadingOnes
               t2 <- T.cppCast t1 T.Signed
               t3 <- T.cppNeg t2
               t4 <- T.cppCond lhsUpperLt0 lower1 int32Min
               T.cppMax t4 t3

  -- lines 868-889
  lowerEnd <- T.cppCond lhsAndrhsLowerGte0
              lower0
              -- we do rhs first cause lhs is a fall through if
              (T.cppCond rhsUpperLt0 lower2 (T.cppCond lhsUpperLt0 lower1 int32Min))
  upperEnd <- T.cppCond lhsAndrhsLowerGte0
              upper0
              (T.cppCond rhsUpperLt0 neg1 (T.cppCond lhsUpperLt0 neg1 int32Max))

  --

  resultLower <- T.cppCond lhsEqAndlhsLowerEq0 (lower _rhs)
                   (T.cppCond lhsEqAndlhsLowerEqNeg1 (lower _lhs)
                     (T.cppCond rhsEqAndrhsLowerEq0 (lower _lhs)
                        (T.cppCond rhsEqAndrhsLowerEqNeg1 (lower _rhs) lowerEnd)))

  resultUpper <- T.cppCond lhsEqAndlhsLowerEq0 (upper _rhs)
                   (T.cppCond lhsEqAndlhsLowerEqNeg1 (upper _lhs)
                     (T.cppCond rhsEqAndrhsLowerEq0 (upper _lhs)
                        (T.cppCond rhsEqAndrhsLowerEqNeg1 (upper _rhs) upperEnd)))

  T.vassign (lower result) resultLower
  T.vassign (upper result) resultUpper
  return result

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
  result <- resultRange T.Signed "result"
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

  result <- resultRange T.Signed "result"

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
  result <- resultRange T.Signed "result"
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

  result <- resultRange T.Unsigned "result"
  T.cppCond isNegOrNonNeg trueLower falseLower >>= T.vassign (lower result)
  T.cppCond isNegOrNonNeg trueUpper falseUpper >>= T.vassign (upper result)
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1042
lsh' :: Range -> Range -> T.Verif Range
lsh' _ _ = do
  -- Trivially correct
  result <- resultRange T.Signed "result"
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

  result <- resultRange T.Signed "result"
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

  result <- resultRange T.Unsigned "result"
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
