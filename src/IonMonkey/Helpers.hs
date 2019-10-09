{-# LANGUAGE FlexibleInstances     #-}
module IonMonkey.Helpers ( setRange
                         , noInt32LowerBound
                         , noInt32UpperBound
                         , isFiniteNonNegative
                         , isFiniteNegative
                         , maxFiniteExponent
                         , includesInfinityAndNan
                         , countLeadingZeroes32
                         , countTrailingZeroes32
                         ) where
import           Control.Monad              (when)
import           Control.Monad.State.Strict
import qualified DSL.DSL                    as D
import qualified DSL.Typed                  as T
import           IonMonkey.Objects

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#370
-- https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#621
setRange :: T.VNode -- ^ Lower
         -> T.VNode -- ^ Upper
         -> T.VNode -- ^ Fractional
         -> T.VNode -- ^ Negative zero
         -> T.VNode -- ^ Exponent
         -> Range
         -> D.Verif ()
setRange low up fract nzero exp r = do
  T.vassign (maxExponent r) exp
  T.vassign (canHaveFractionalPart r) fract
  T.vassign (canBeNegativeZero r) nzero
  setLowerInit low r
  setUpperInit up r
  optimize r

optimize :: Range
         -> D.Verif ()
optimize _ = return ()

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#252
setLowerInit :: T.VNode
             -> Range
             -> D.Verif ()
setLowerInit lower_ range = do
  when (T.vtype lower_ /= T.Signed64) $ error "Expected a signed 64-bit lower"
  min <- jsValIntMin
  max <- jsValIntMax
  castIntMin <- T.cppCast min T.Signed64 --jsValIntMin64
  castIntMax <- T.cppCast max T.Signed64 --jsValIntMax64
  t <- T.true
  f <- T.false
  -- The default values:
  -- else
  --  lower_ = int32_t(x);
  --  hasInt32LowerBound_ = true;
  defaultLower <- T.cppCast lower_ T.Signed
  let defaultHasLower = t
  -- if (x > JSVAL_INT_MAX)
  --    lower_ = JSVAL_INT_MAX;
  --    hasInt32LowerBound_ = true;)
  oobUpper <- T.cppGt lower_ castIntMax
  lower' <- T.cppCond oobUpper max defaultLower
  hasLower' <- T.cppCond oobUpper t defaultHasLower
  -- else if (x < JSVAL_INT_MIN)
  --     lower_ = JSVAL_INT_MIN
  --     hasInt32LowerBound_ = false
  oobLower <- T.cppLt lower_ castIntMin
  lower'' <- T.cppCond oobLower min lower'
  hasLower'' <- T.cppCond oobLower f hasLower'
  -- Set the flag and the bound
  T.vassign lower'' (lower range)
  T.vassign hasLower'' (hasInt32LowerBound range)

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#265
setUpperInit :: T.VNode
             -> Range
             -> D.Verif ()
setUpperInit upper_ range = do
  when (T.vtype upper_ /= T.Signed64) $ error "Expected a signed 64-bit lower"
  min <- jsValIntMin
  max <- jsValIntMax
  castIntMin <- T.cppCast min T.Signed64 --jsValIntMin64
  castIntMax <- T.cppCast max T.Signed64 --jsValIntMax64
  t <- T.true
  f <- T.false
  -- DEFAULT
  -- else
  --     upper_ = int32_t(x);
  --     hasInt32UpperBound_ = true;
  defaultUpper <- T.cppCast upper_ T.Signed
  let defaultHasUpper = t
  -- if (x > JSVAL_INT_MAX)
  --     upper_ = JSVAL_INT_MAX;
  --     hasInt32UpperBound_ = false;
  oobUpper <- T.cppGt upper_ castIntMax
  upper' <- T.cppCond oobUpper max defaultUpper
  hasLower' <- T.cppCond oobUpper f defaultHasUpper
  -- else if (x < JSVAL_INT_MIN)
  --     upper_ = JSVAL_INT_MIN;
  --     hasInt32UpperBound_ = true;
  oobLower <- T.cppLt upper_ castIntMin
  upper'' <- T.cppCond oobLower min upper'
  hasLower'' <- T.cppCond oobLower t hasLower'
  -- Set the flags and the bound
  T.vassign upper'' (upper range)
  T.vassign hasLower'' (hasInt32UpperBound range)

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#154
-- static const int64_t NoInt32LowerBound = int64_t(JSVAL_INT_MIN) - 1;
noInt32LowerBound :: D.Verif T.VNode
noInt32LowerBound = do
  jsMin <- jsValIntMin
  jsMinExt <- T.cppCast jsMin T.Signed64
  one <- T.num64 1
  T.cppSub jsMinExt one

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#153
-- static const int64_t NoInt32UpperBound = int64_t(JSVAL_INT_MAX) + 1;
noInt32UpperBound :: D.Verif T.VNode
noInt32UpperBound = do
  jsMax <- jsValIntMax
  jsMaxExt <- T.cppCast jsMax T.Signed64
  one <- T.num64 1
  T.cppAdd jsMaxExt one

-- | https://searchfox.org/mozilla-central/source/js/public/Value.h#35
-- #define JSVAL_INT_MIN ((int32_t)0x80000000)
jsValIntMin :: D.Verif T.VNode
jsValIntMin = T.num 0x80000000

jsValIntMin64 :: D.Verif T.VNode
jsValIntMin64 = T.num64 (-2147483648)

-- | https://searchfox.org/mozilla-central/source/js/public/Value.h#36
-- #define JSVAL_INT_MAX ((int32_t)0x7fffffff)
jsValIntMax :: D.Verif T.VNode
jsValIntMax = T.num 0x7fffffff

jsValIntMax64 :: D.Verif T.VNode
jsValIntMax64 = T.num64 2147483647

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#136
--  static const uint16_t MaxFiniteExponent = mozilla::FloatingPoint<double>::kExponentBias;
-- TODO: WHAT IS THIS????? MLFB
maxFiniteExponent :: D.Verif T.VNode
maxFiniteExponent = T.unum16 4

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#145
-- static const uint16_t IncludesInfinityAndNaN = UINT16_MAX;
-- #   define UINT16_MAX      ((uint16_t)(65535U))
includesInfinityAndNan :: D.Verif T.VNode
includesInfinityAndNan = T.unum16 65535

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#551
isFiniteNonNegative :: Range -> D.Verif T.VNode
isFiniteNonNegative range = do
  zero <- T.num 0
  T.cppGte (lower range) zero

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#548
isFiniteNegative :: Range -> D.Verif T.VNode
isFiniteNegative range = do
  zero <- T.num 0
  T.cppLt (upper range) zero

countOnes :: T.VNode -> D.Verif T.VNode
countOnes num = do
  one <- T.num 1
  two <- T.num 2
  four <- T.num 4
  eight <- T.num 8
  sixteen <- T.num 16
  fives <- T.num 1431655765
  threes <- T.num 858993459
  f0s <- T.num 252645135
  sixtythree <- T.num 63

  -- count
  -- x -= x >> 1 & 0x55555555;
  shift1 <- T.cppShiftRight num one
  and1 <- T.cppAnd shift1 fives
  tmp1 <- T.cppSub num and1

  -- x = (x >> 2 & 0x33333333) + (x & 0x33333333);
  shift2 <- T.cppShiftRight tmp1 two
  and2 <- T.cppAnd shift2 threes
  and3 <- T.cppAnd tmp1 threes
  tmp2 <- T.cppAdd and2 and3

  -- x = (x >> 4) + x & 0x0f0f0f0f;
  shift3 <- T.cppShiftRight tmp2 four
  add1 <- T.cppAdd shift3 tmp2
  tmp3 <- T.cppAdd add1 f0s

  -- x += x >> 8;
  shift4 <- T.cppShiftRight tmp3 eight
  tmp4 <- T.cppAdd tmp3 shift4

  -- x += x >> 16;
  shift5 <- T.cppShiftRight tmp4 sixteen
  tmp5 <- T.cppAdd tmp4 shift5

  -- return numIntBits - (x & 0x0000003f); //subtract # of 1s from 32)
  T.cppAnd tmp5 sixtythree

class CountLeadingZeroes32 n where
  countLeadingZeroes32 :: n -> D.Verif T.VNode

instance CountLeadingZeroes32 (D.Verif T.VNode) where
  countLeadingZeroes32 act = act >>= countLeadingZeroes32 

instance CountLeadingZeroes32 T.VNode where
  countLeadingZeroes32 node = do
      -- https://stackoverflow.com/questions/10439242/count-leading-zeroes-in-an-int32
      -- const int numIntBits = sizeof(int) * 8; //compile time constant
      -- //do the smearing
      -- x |= x >> 1;
      -- x |= x >> 2;
      -- x |= x >> 4;
      -- x |= x >> 8;
      -- x |= x >> 16;
    numBits <- T.num 32
    one <- T.num 1
    two <- T.num 2
    four <- T.num 4
    eight <- T.num 8
    sixteen <- T.num 16
    thirtytwo <- T.num 32

    -- smear (ew)
    tmp1 <- T.cppShiftRight node one >>= T.cppOr node
    tmp2 <- T.cppShiftRight tmp1 two >>= T.cppOr tmp1
    tmp3 <- T.cppShiftRight tmp2 four >>= T.cppOr tmp2
    tmp4 <- T.cppShiftRight tmp3 eight >>= T.cppOr tmp3
    tmp5 <- T.cppShiftRight tmp4 sixteen >>= T.cppOr tmp4

    -- //count the ones
    numOnes <- countOnes tmp5
    T.cppSub thirtytwo numOnes

countTrailingZeroes32 :: T.VNode -> D.Verif T.VNode
countTrailingZeroes32 node = undefined
