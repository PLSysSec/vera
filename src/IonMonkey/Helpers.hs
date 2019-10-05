module IonMonkey.Helpers ( noInt32LowerBound
                         , noInt32UpperBound
                         , isFiniteNonNegative
                         , isFiniteNegative
                         , maxFiniteExponent
                         , includesInfinityAndNan
                         , countLeadingZeroes32
                         , countTrailingZeroes32
                         ) where
import qualified DSL.DSL           as D
import qualified DSL.Typed         as T
import           IonMonkey.Objects

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

-- | https://searchfox.org/mozilla-central/source/js/public/Value.h#36
-- #define JSVAL_INT_MAX ((int32_t)0x7fffffff)
jsValIntMax :: D.Verif T.VNode
jsValIntMax = T.num 0x7fffffff

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#136
--  static const uint16_t MaxFiniteExponent = mozilla::FloatingPoint<double>::kExponentBias;
--
maxFiniteExponent :: D.Verif T.VNode
maxFiniteExponent = undefined

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
  numBits <- T.num 32
  one <- T.num 1
  two <- T.num 2
  four <- T.num 4
  eight <- T.num 8
  sixteen <- T.num 16
  fives <- T.num 1431655765
  threes <- T.num 858993459
  fs <- T.num 252645135
  sixtythree <- T.num 63

  -- count
  -- x -= x >> 1 & 0x55555555;
  right <- T.cppAnd one fives
  total <- T.cppShiftRight num right
  tmp1 <- T.cppSub num total
  -- x = (x >> 2 & 0x33333333) + (x & 0x33333333);
  firstAnd <- T.cppAnd two threes
  left <- T.cppShiftRight tmp1 firstAnd
  right <- T.cppAnd tmp1 threes
  tmp2 <- T.cppAnd left right
  -- x = (x >> 4) + x & 0x0f0f0f0f;
  left <- T.cppShiftRight tmp2 four
  right <- T.cppAnd tmp2 fs
  tmp3 <- T.cppAnd left right
  -- x += x >> 8;
  shift <- T.cppShiftRight tmp3 eight
  tmp4 <- T.cppAdd tmp3 shift
  -- x += x >> 16;
  shift2 <- T.cppShiftRight tmp4 sixteen
  tmp5 <- T.cppAdd tmp4 shift2
  -- return numIntBits - (x & 0x0000003f); //subtract # of 1s from 32)
  mask <- T.cppAnd tmp5 sixtythree
  T.cppSub numBits mask

countLeadingZeroes32 :: T.VNode -> D.Verif T.VNode
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

  -- smear (ew)
  tmp1 <- T.cppShiftRight node one >>= T.cppOr node
  tmp2 <- T.cppShiftRight tmp1 two >>= T.cppOr tmp1
  tmp3 <- T.cppShiftRight tmp2 four >>= T.cppOr tmp2
  tmp4 <- T.cppShiftRight tmp3 eight >>= T.cppOr tmp3
  tmp5 <- T.cppShiftRight tmp4 sixteen >>= T.cppOr tmp4

  -- //count the ones
  countOnes tmp5

countTrailingZeroes32 :: T.VNode -> D.Verif T.VNode
countTrailingZeroes32 node = undefined
