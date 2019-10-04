module IonMonkey.Helpers ( isFiniteNonNegative
                         , isFiniteNegative
                         , countLeadingZeroes32
                         , countTrailingZeroes32
                         ) where
import qualified DSL.DSL           as D
import qualified DSL.Typed         as T
import           IonMonkey.Objects

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#483
hasInt32LowerBound :: Range -> D.Verif T.VNode
hasInt32LowerBound range = hasInt32LowerBound range

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#484
hasInt32UpperBound :: Range -> D.Verif T.VNode
hasInt32UpperBound range = hasInt32UpperBound range

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#154
-- static const int64_t NoInt32LowerBound = int64_t(JSVAL_INT_MIN) - 1;
noInt32LowerBound :: D.Verif T.VNode
noInt32LowerBound = error "not done yet, need int64"

-- | #define JSVAL_INT_MIN ((int32_t)0x80000000)
jsValIntMin :: D.Verif T.VNode
jsValIntMin = T.num 0x80000000

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

countLeadingZeroes32 :: D.Node -> D.Verif D.Node
countLeadingZeroes32 node = do
    -- https://stackoverflow.com/questions/10439242/count-leading-zeroes-in-an-int32
    -- const int numIntBits = sizeof(int) * 8; //compile time constant
    -- //do the smearing
    -- x |= x >> 1;
    -- x |= x >> 2;
    -- x |= x >> 4;
    -- x |= x >> 8;
    -- x |= x >> 16;
    -- //count the ones
    -- x -= x >> 1 & 0x55555555;
    -- x = (x >> 2 & 0x33333333) + (x & 0x33333333);
    -- x = (x >> 4) + x & 0x0f0f0f0f;
    -- x += x >> 8;
    -- x += x >> 16;
    -- return numIntBits - (x & 0x0000003f); //subtract # of 1s from 32
  numBits <- D.i32c 32
  one <- D.i32c 1
  two <- D.i32c 2
  four <- D.i32c 4
  eight <- D.i32c 8
  sixteen <- D.i32c 16
  fives <- D.i32c 1431655765
  threes <- D.i32c 858993459
  fs <- D.i32c 252645135
  sixtythree <- D.i32c 63
  -- smear (ew)
  tmp1 <- D.safeSra node one >>= D.or node
  tmp2 <- D.safeSra tmp1 two >>= D.or tmp1
  tmp3 <- D.safeSra tmp2 four >>= D.or tmp2
  tmp4 <- D.safeSra tmp3 eight >>= D.or tmp3
  tmp5 <- D.safeSra tmp4 sixteen >>= D.or tmp4
  -- count
  -- x -= x >> 1 & 0x55555555;
  right <- D.and one fives
  total <- D.safeSra tmp5 right
  tmp6 <- D.sub tmp5 total
  -- x = (x >> 2 & 0x33333333) + (x & 0x33333333);
  firstAnd <- D.and two threes
  left <- D.safeSra tmp6 firstAnd
  right <- D.and tmp6 threes
  tmp7 <- D.add left right
  -- x = (x >> 4) + x & 0x0f0f0f0f;
  left <- D.safeSra tmp7 four
  right <- D.and tmp7 fs
  tmp8 <- D.add left right
  -- x += x >> 8;
  shift <- D.safeSra tmp8 eight
  tmp9 <- D.add tmp8 shift
  -- x += x >> 16;
  shift2 <- D.safeSra tmp9 sixteen
  tmp10 <- D.add tmp9 shift2
  -- return numIntBits - (x & 0x0000003f); //subtract # of 1s from 32)
  mask <- D.and tmp10 sixtythree
  D.sub numBits mask

countTrailingZeroes32 :: D.Node -> D.Verif D.Node
countTrailingZeroes32 node = undefined




