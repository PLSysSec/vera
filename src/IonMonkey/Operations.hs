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
                            , floor
                            , ceil
                            , sign
                            , nanToZero
                            ) where
import           Control.Monad.State.Strict (liftIO)
import           Data.Maybe                 (fromJust)
import qualified DSL.Typed                  as T
import           IonMonkey.Helpers
import           IonMonkey.Objects
import           Prelude                    hiding (abs, and, ceil, floor, max,
                                             min, not, or)

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
    one <- T.unum16 1
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
  setRange l h fractFlag negZeroFlag e result
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#775
sub :: Range -> Range -> T.Verif Range
sub left right = do

  newLower <- do
    -- setup the results
    trueLower <- noInt32LowerBound
    castLower <- T.cppCast (lower left) T.Signed64
    castUpper <- T.cppCast (upper right) T.Signed64
    falseLower <- T.cppSub castLower castUpper
    -- make the cond
    cond <- T.cppOr (hasInt32LowerBound left) (hasInt32UpperBound right)
    T.cppCond cond trueLower falseLower

  newUpper <- do
    -- results
    trueUpper <- noInt32UpperBound
    castUpper <- T.cppCast (upper left) T.Signed64
    castLower <- T.cppCast (lower right) T.Signed64
    falseUpper <- T.cppSub castUpper castLower
    -- cond
    cond <- T.cppOr (hasInt32UpperBound left) (hasInt32LowerBound right)
    T.cppCond cond trueUpper falseUpper

  e <- do
    tmpExp <- do
      tmpExp <- T.cppMax (maxExponent left) (maxExponent right)
      tmpExpPlus <- T.unum16 1 >>= T.cppAdd tmpExp
      maxExp <- maxFiniteExponent
      -- return the right exp
      tmpExpLtMaxExp <- T.cppLte tmpExp maxExp
      T.cppCond tmpExpLtMaxExp tmpExpPlus tmpExp
    nanExp <- includesInfinityAndNan

    cond <- do
      lInf <- canBeInfiniteOrNan' left
      rInf <- canBeInfiniteOrNan' right
      T.cppAnd lInf rInf

    T.cppCond cond nanExp tmpExp

  fract <- T.cppOr (canHaveFractionalPart left) (canHaveFractionalPart right)
  nz <- T.cppAnd (canBeNegativeZero left) (canBeNegativeZero right)

  result <- resultRange T.Double "result"
  setRange newLower newUpper fract nz e result
  return result

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

  -- Compute the lower and upper, lines 865-890

  -- 879-880
  lower1 <- do leadingOnes <- countLeadingZeroes32 $ T.cppNot (lower _lhs)
               T.cppMax int32Min (T.cppNot $ T.cppCast (T.cppShiftRight uint32Max leadingOnes) T.Signed)

  -- 884-885
  lower2 <- do leadingOnes <- countLeadingZeroes32 $ T.cppNot (lower _rhs)
               T.cppMax (T.cppCond (T.cppLt (upper _lhs) zero)
                          lower1
                          int32Min)
                        (T.cppNot $ T.cppCast (T.cppShiftRight uint32Max leadingOnes) T.Signed)

  -- lines 868-889
  lowerEnd <- T.cppCond ((T.cppGte (lower _lhs) zero) `T.cppAnd` (T.cppGte (lower _rhs) zero))
                (T.cppMax (lower _lhs) (lower _rhs))
                -- we do rhs first cause lhs is a fall through if
                (T.cppCond (T.cppLt (upper _rhs) zero)
                      lower2
                      (T.cppCond (T.cppLt (upper _lhs) zero)
                        lower1
                        int32Min))
  upperEnd <- T.cppCond ((T.cppGte (lower _lhs) zero) `T.cppAnd` (T.cppGte (lower _rhs) zero))
                 (T.cppCast (T.cppShiftRight uint32Max
                              (T.cppMin (T.named "ctlzUpperLhs" $ countLeadingZeroes32 $ upper _lhs)
                                        (T.named "ctlzUpperRhs" $ countLeadingZeroes32 $ upper _rhs))) T.Signed)
                (T.cppCond ((T.cppLt (upper _lhs) zero)  `T.cppOr` (T.cppLt (upper _lhs) zero))
                      neg1
                      int32Max)
  --
  resultLower <-
      T.cppCond ((T.cppEq (lower _lhs) (upper _lhs)) `T.cppAnd` (T.cppEq (lower _lhs) zero)) (lower _rhs) $
      T.cppCond ((T.cppEq (lower _lhs) (upper _lhs)) `T.cppAnd` (T.cppEq (lower _lhs) neg1)) (lower _lhs) $
      T.cppCond ((T.cppEq (lower _rhs) (upper _rhs)) `T.cppAnd` (T.cppEq (lower _rhs) zero)) (lower _lhs) $
      T.cppCond ((T.cppEq (lower _rhs) (upper _rhs)) `T.cppAnd` (T.cppEq (lower _rhs) neg1)) (lower _rhs)
      lowerEnd
  resultUpper <-
      T.cppCond ((T.cppEq (lower _lhs) (upper _lhs)) `T.cppAnd` (T.cppEq (lower _lhs) zero)) (upper _rhs) $
      T.cppCond ((T.cppEq (lower _lhs) (upper _lhs)) `T.cppAnd` (T.cppEq (lower _lhs) neg1)) (upper _lhs) $
      T.cppCond ((T.cppEq (lower _rhs) (upper _rhs)) `T.cppAnd` (T.cppEq (lower _rhs) zero)) (upper _lhs) $
      T.cppCond ((T.cppEq (lower _rhs) (upper _rhs)) `T.cppAnd` (T.cppEq (lower _rhs) neg1)) (upper _rhs)
      upperEnd


  T.vassign (lower result) resultLower
  T.vassign (upper result) resultUpper
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#893
xor _lhs _rhs = undefined
  -- zero <- T.num 0
  --
  -- -- 906-917
  -- lhsLower <- T.cppCond (T.cppLt (upper _lhs) zero)
  --               (T.cppNot $ upper _lhs)
  --               (lower _lhs)
  -- lhsUpper <- T.cppCond (T.cppLt (upper _lhs) zero)
  --               (T.cppNot $ lower _lhs)
  --               (upper _lhs)
  -- rhsLower <- T.cppCond (T.cppLt (upper _rhs) zero)
  --               (T.cppNot $ upper _rhs)
  --               (lower _rhs)
  -- rhsUpper <- T.cppCond (T.cppLt (upper _rhs) zero)
  --               (T.cppNot $ lower _rhs)
  --               (upper _rhs)
  -- invertAfter <- T.cppCond ((T.cppLt (upper _lhs) zero) `T.cppAnd` (T.cppLt (upper _rhs) zero))
  --                   false
  --                   ((T.cppLt (upper _lhs) zero) `T.cppOr` (T.cppLt (upper _rhs) zero))


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
mul :: Range -> Range -> T.Verif Range
mul left right = do

  fract <- T.cppOr (canHaveFractionalPart left) (canHaveFractionalPart right)

  nzf <- do

    lSign <- canHaveSignBitSet left
    rNeg <- canBeFiniteNonNegative right
    topLine <- T.cppAnd lSign rNeg

    rSign <- canHaveSignBitSet right
    lNeg <- canBeFiniteNonNegative left
    bottomLine <- T.cppAnd rSign lNeg

    T.cppOr topLine bottomLine

  -- the exponent
  e <- do
    ifCond <- do
      lInf <- canBeInfiniteOrNan' left
      rInf <- canBeInfiniteOrNan' right
      T.cppAnd lInf rInf
    ifResult <- do
      -- Get the variables
      numLeft <- numBits left
      numRight <- numBits right
      one <- T.unum16 1
      -- Do the arithmetic for the conditional
      added <- T.cppAdd numLeft numRight
      tmpExp <- T.cppSub added one
      -- Make the actual conditional
      maxExp <- maxFiniteExponent
      cond <- T.cppGt tmpExp maxExp
      -- If the conditional is true, return includes infinity
      includesInf <- includesInfinity
      T.cppCond cond includesInf maxExp

    elseIfCond <- do
      firstLine <- do
        lNan <- canBeNan left >>= T.cppNot
        rNan <- canBeNan right >>= T.cppNot
        T.cppAnd lNan rNan

      secondLine <- do
        lZero <- canBeZero left
        rInf <- canBeInfiniteOrNan' right
        T.cppAnd lZero rInf >>= T.cppNot

      thirdLine <- do
        rZero <- canBeZero right
        lInf <- canBeInfiniteOrNan' left
        T.cppAnd rZero lInf >>= T.cppNot

      T.cppAnd firstLine secondLine >>= T.cppAnd thirdLine
    elseIfResult <- includesInfinity

    elseResult <- includesInfinityAndNan

    elseBrCond <- T.cppCond elseIfCond elseIfResult elseResult
    T.cppCond ifCond ifResult elseBrCond

  -- Now do the upper and lower: tmp variables
  missingInt32Bounds <- missingAnyInt32Bounds left right
  castLeftLower <- T.cppCast (lower left) T.Signed64
  castLeftUpper <- T.cppCast (upper left) T.Signed64
  castRightLower <- T.cppCast (lower right) T.Signed64
  castRightUpper <- T.cppCast (upper right) T.Signed64
  a <- T.cppMul castLeftLower castRightLower
  b <- T.cppMul castLeftLower castRightUpper
  c <- T.cppMul castLeftUpper castRightLower
  d <- T.cppMul castLeftUpper castRightUpper

  newLower <- do

    trueBr <- noInt32LowerBound

    falseBr <- do
      tmp1 <- T.cppMin a b
      tmp2 <- T.cppMin c d
      T.cppMin tmp1 tmp2

    T.cppCond missingInt32Bounds trueBr falseBr

  newUpper <- do

    trueBr <- noInt32UpperBound

    falseBr <- do
      tmp1 <- T.cppMax a b
      tmp2 <- T.cppMax c d
      T.cppMax tmp1 tmp2

    T.cppCond missingInt32Bounds trueBr falseBr

  result <- resultRange T.Double "result"
  setRange newLower newUpper fract nzf e result
  return result


-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#999
lsh :: Range -> T.VNode -> T.Verif Range
lsh shiftee val = do
  -- Setup the shift
  thirtyOne <- T.num 31
  one <- T.num 1
  shift <- T.cppAnd thirtyOne val

  -- Desired range
  castLower <- T.cppCast (lower shiftee) T.Unsigned
  castUpper <- T.cppCast (upper shiftee) T.Unsigned
  shiftedLower <- T.cppShiftLeft castLower shift
  shiftedUpper <- T.cppShiftLeft castUpper shift

  -- Compute the branch conditions
  doesntLoseBits <- do
    lowerDoesntLoseBits <- do
      tmp1 <- T.cppShiftLeft shiftedLower one
      tmp2 <- T.cppShiftRight tmp1 shift
      tmp3 <- T.cppShiftRight tmp2 one
      castTmp <- T.cppCast tmp3 T.Signed
      T.cppEq castTmp (lower shiftee)

    upperDoesntLoseBits <- do
      tmp1 <- T.cppShiftLeft shiftedUpper one
      tmp2 <- T.cppShiftRight tmp1 shift
      tmp3 <- T.cppShiftRight tmp2 one
      castTmp <- T.cppCast tmp3 T.Signed
      T.cppEq castTmp (upper shiftee)

    T.cppAnd lowerDoesntLoseBits upperDoesntLoseBits

  result <- resultRange T.Signed "result"

  -- fallback range
  i32min <- T.num (-2147483648) -- WHY ARE THESE NOT SHOWING UP AS DIFFERENT
  i32max <- T.num 2147483647

  -- They get cast back on assignment
  -- https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1009
  shiftedLowerSigned <- T.cppCast shiftedLower T.Signed
  shiftedUpperSigned <- T.cppCast shiftedUpper T.Signed

  T.cppCond doesntLoseBits shiftedLowerSigned i32min >>= T.vassign (lower result)
  T.cppCond doesntLoseBits shiftedUpperSigned i32max >>= T.vassign (upper result)

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
abs :: Range -> T.Verif Range
abs op = do

  -- FractionalPartFlag canHaveFractionalPart = op->canHaveFractionalPart_;
  let fract = canHaveFractionalPart op
  -- // Abs never produces a negative zero.
  nz <- T.false

  -- Setup for later calculations
  zero <- T.num 0
  t <- T.true
  intMin <- T.intMin
  intMax <- T.intMax

  -- Max(Max(int32_t(0), l), u == INT32_MIN ? INT32_MAX : -u)
  lb <- do
    max1 <- T.cppMax (lower op) zero
    max2 <- do
      cond <- T.cppEq (upper op) intMin
      let trueBr = intMax
      falseBr <- T.cppNeg (upper op)
      T.cppCond cond trueBr falseBr
    T.cppMax max1 max2

  let hasLower = t

  -- Max(Max(int32_t(0), u), l == INT32_MIN ? INT32_MAX : -l)
  ub <- do
    max1 <- T.cppMax (upper op) zero
    max2 <- do
      cond <- T.cppEq (lower op) intMin
      let trueBr = intMax
      falseBr <- T.cppNeg (lower op)
      T.cppCond cond trueBr falseBr
    T.cppMax max1 max2

  -- op->hasInt32Bounds() && l != INT32_MIN
  hasUpper <- do
    notLowest <- T.cppEq (lower op) intMin >>= T.cppNot
    T.cppAnd (hasInt32LowerBound op) (hasInt32UpperBound op) >>= T.cppAnd notLowest

  let e = maxExponent op

  -- Raw initialize
  --   Range(Max(Max(int32_t(0), l), u == INT32_MIN ? INT32_MAX : -u), true,
  --            Max(Max(int32_t(0), u), l == INT32_MIN ? INT32_MAX : -l),
  --            op->hasInt32Bounds() && l != INT32_MIN, canHaveFractionalPart,
  --            canBeNegativeZero, op->max_exponent_);)
  result <- resultRange T.Double "result"
  T.vassign (canHaveFractionalPart result) fract
  T.vassign (canBeNegativeZero result) nz
  T.vassign (lower result) lb
  T.vassign (hasInt32LowerBound result) hasLower
  T.vassign (upper result) ub
  T.vassign (hasInt32UpperBound result) hasUpper
  T.vassign (maxExponent result) e
  return result


-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1104
min :: Range -> Range -> T.Verif Range
min left right = do

  -- FractionalPart = lhs fract || rhs fract
  fract <- T.cppOr (canHaveFractionalPart left) (canHaveFractionalPart right)
  -- Nz = lhs nz || rhs nz
  nz <- T.cppOr (canBeNegativeZero left) (canBeNegativeZero right)
  -- Min(lhs lower, rhs lower), rhs has lower && rhs has lower
  newLower <- T.cppMin (lower left) (lower right)
  hasLower <- T.cppAnd (hasInt32LowerBound left) (hasInt32UpperBound right)
  -- min(lhs upper, rhs upper), lhs has upper || rhs has upper
  newUpper <- T.cppMin (lower right) (upper right)
  hasUpper <- T.cppOr (hasInt32UpperBound left) (hasInt32LowerBound right)
  -- max(lhs exp, rhs exp)
  exp <- T.cppMax (maxExponent left) (maxExponent right)
  -- Raw initialize
  result <- resultRange T.Double "result"
  T.vassign (canHaveFractionalPart result) fract
  T.vassign (canBeNegativeZero result) nz
  T.vassign (lower result) newLower
  T.vassign (hasInt32LowerBound result) hasLower
  T.vassign (upper result) newUpper
  T.vassign (hasInt32UpperBound result) hasUpper
  T.vassign (maxExponent result) exp
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1123
max :: Range -> Range -> T.Verif Range
max left right = do

  -- fract part = lhs fract || rhs fract
  fract <- T.cppOr (canHaveFractionalPart left) (canHaveFractionalPart right)
  -- negz = lhs nz || rhs nz
  nz <- T.cppOr (canBeNegativeZero left) (canBeNegativeZero right)
  -- lb = max(lhs lower, rhs lower), lhs haslower || rhs haslower
  newLower <- T.cppMax (lower left) (lower right)
  hasLower <- T.cppOr (hasInt32LowerBound left) (hasInt32LowerBound right)
  -- ub = max(lhs upper, rhs upper), lhs hasupper && rhs hasupper
  newUpper <- T.cppMax (upper left) (upper right)
  hasUpper <- T.cppAnd (hasInt32UpperBound left) (hasInt32UpperBound right)
  -- max(lhs exp, rhs exp)
  exp <- T.cppMax (maxExponent left) (maxExponent right)
  -- Raw initialize
  result <- resultRange T.Double "result"
  T.vassign (canHaveFractionalPart result) fract
  T.vassign (canBeNegativeZero result) nz
  T.vassign (lower result) newLower
  T.vassign (hasInt32LowerBound result) hasLower
  T.vassign (upper result) newUpper
  T.vassign (hasInt32UpperBound result) hasUpper
  T.vassign (maxExponent result) exp
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1142
floor :: Range -> T.Verif Range
floor op = do

  -- if (op->canHaveFractionalPart() && op->hasInt32LowerBound())
  --            copy->setLowerInit(int64_t(copy->lower_) - 1);

  cond <- T.cppAnd (canHaveFractionalPart op) (hasInt32LowerBound op)
  (trueLower, trueHasLower) <- do
    one64 <- T.num64 1
    extLower <- T.cppCast (lower op) T.Signed64
    arg <- T.cppSub extLower one64
    setLowerInit' arg

  newLower <- T.cppCond cond trueLower (lower op)
  newHasLower <- T.cppCond cond trueHasLower (hasInt32LowerBound op)

  maxExp <- maxFiniteExponent

  -- if (copy->hasInt32Bounds())
  --    copy->max_exponent_ = copy->exponentImpliedByInt32Bounds();)
  -- else if (copy->max_exponent_ < MaxFiniteExponent)
  --      copy->max_exponent_++;
  -- Create the conditions
  hasInt32Bound <- T.cppAnd (hasInt32LowerBound op) (hasInt32UpperBound op)
  expRoom <- T.cppLt (maxExponent op) maxExp
  -- Create the results
  ifResult <- exponentImpliedByInt32Bounds op
  elseIfResult <- do
    one16 <- T.unum 1
    T.cppAdd (maxExponent op) one16
  let elseResult = maxExponent op

  elseAndElseIfResult <- T.cppCond expRoom elseIfResult elseResult
  e <- T.cppCond hasInt32Bound ifResult elseAndElseIfResult

  fractPart <- T.false

  -- The result is just a copy of the original with a new exponent, lower, haslower, and fract
  result <- resultRange T.Double "result"
  T.vassign (lower result) newLower
  T.vassign (upper result) (upper op)
  T.vassign (hasInt32LowerBound result) newHasLower
  T.vassign (hasInt32UpperBound result) (hasInt32UpperBound op)
  T.vassign (canBeInfiniteOrNan result) (canBeInfiniteOrNan op)
  T.vassign (canBeNegativeZero result) (canBeNegativeZero op)
  T.vassign (canHaveFractionalPart result) fractPart
  T.vassign (maxExponent result) e
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1166
ceil :: Range -> T.Verif Range
ceil op = do
  -- What we're trying to do:
  -- if (copy->hasInt32Bounds())
  --    copy->max_exponent_ = copy->exponentImpliedByInt32Bounds();)
  -- else if (copy->max_exponent_ < MaxFiniteExponent)
  --      copy->max_exponent_++;
  maxExp <- maxFiniteExponent
  -- Conditions
  hasInt32Bound <- T.cppAnd (hasInt32LowerBound op) (hasInt32UpperBound op)
  expRoom <- T.cppLt (maxExponent op) maxExp
  -- Result
  ifResult <- exponentImpliedByInt32Bounds op
  elseIfResult <- do
    one16 <- T.unum 1
    T.cppAdd (maxExponent op) one16
  let elseResult = maxExponent op

  elseAndElseIfResult <- T.cppCond expRoom elseIfResult elseResult
  e <- T.cppCond hasInt32Bound ifResult elseAndElseIfResult

  fract <- T.false

  -- The result is a copy of the original
  result <- resultRange T.Double "result"
  T.vassign (lower result) (lower op)
  T.vassign (upper result) (upper op)
  T.vassign (hasInt32LowerBound result) (hasInt32LowerBound op)
  T.vassign (hasInt32UpperBound result) (hasInt32UpperBound op)
  T.vassign (canBeInfiniteOrNan result) (canBeInfiniteOrNan op)
  T.vassign (canBeNegativeZero result) (canBeNegativeZero op)
  T.vassign (canHaveFractionalPart result) fract
  T.vassign (maxExponent result) e
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1184
sign :: Range -> T.Verif Range
sign op = do
  one <- T.num 1
  negOne <- T.num (-1)
  zero <- T.num 0

  -- Max(Min(op->lower_, 1), -1)
  low <- T.cppMin (lower op) one >>= T.cppMax negOne
  -- Max(Min(op->upper_, 1)
  up <- T.cppMin (upper op) one >>= T.cppMax negOne
  -- ExcludesFractionalParts is just false
  fract <- T.false
  let nz = canBeNegativeZero op
  e <- T.unum16 0

  -- cast lower and upper to int64 before sending to setRange, which takes
  -- signed 64s as input
  castLower <- T.cppCast low T.Signed64
  castUpper <- T.cppCast up T.Signed64

  result <- resultRange T.Double "result"
  setRange castLower castUpper fract nz e result
  return result

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1195
nanToZero :: Range -> T.Verif Range
nanToZero = undefined

