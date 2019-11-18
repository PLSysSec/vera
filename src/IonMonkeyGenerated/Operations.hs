{-# LANGUAGE QuasiQuotes #-}
module IonMonkeyGenerated.Operations ( add -- FRACT
                                     , sub -- FRACT
                                     , mul -- FRACT
                                     , and -- FRACT
                                     , or -- FRACT
                                     , xor -- FRACT
                                     , not -- FRACT
                                     , lsh -- FRACT
                                     , rsh -- FRACT
                                     , ursh -- needs assumption, FRACT
                                     , lsh' -- FRACT
                                     , rsh' -- FRACT
                                     , ursh' -- needs assumption, FRACT
                                     , abs -- FRACT
                                     , min -- FRACT
                                     , max -- FRACT
                                     , floor -- FRACT. WHY DIDNT THIS VERIFY BEFORE
                                     , ceil -- TEST
                                     , sign -- done
                                     , intersect -- ish
                                     , brokenIntersect
                                     , union
                                     ) where
import           Control.Monad
import           DSL.Typed                  (Type (..))
import           Generate.Lang
import           Generate.QQ
import           IonMonkeyGenerated.Helpers
import           Prelude                    hiding (abs, and, div, floor, max,
                                             min, mod, not, or)

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#744
add :: FunctionDef
add = [funcStr| range add(range lhs, range rhs) {
  int64_t l;
  int64_t h;
  uint16_t e;

  l = (int64_t) lhs.lower + (int64_t) rhs.lower;
  if(!lhs.hasInt32LowerBound | !rhs.hasInt32LowerBound) {
    l = #{noInt32LowerBoundS};
  }

  h = (int64_t) lhs.upper + (int64_t) rhs.upper;
  if(!lhs.hasInt32UpperBound | !rhs.hasInt32UpperBound) {
    h = #{noInt32UpperBoundS};
  }

  e = math::max(lhs.maxExponent, rhs.maxExponent);
  if(e <= #{maxFiniteExponentS}) {
    e += (uint16_t) 1;
  }

  if(canBeInfiniteOrNan(lhs) & canBeInfiniteOrNan(rhs)) {
    e = #{includesInfinityAndNanS};
  }

  return Range4(l,
                h,
                lhs.canHaveFractionalPart | rhs.canHaveFractionalPart,
                lhs.canBeNegativeZero & rhs.canBeNegativeZero,
                e);

}|]

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#775
sub :: FunctionDef
sub = [funcStr| range sub(range lhs, range rhs) {

  int64_t l = (int64_t) lhs.lower - (int64_t) rhs.upper;
  if(!lhs.hasInt32LowerBound | !rhs.hasInt32LowerBound) {
    l = #{noInt32LowerBoundS};
  }

  int64_t h = (int64_t) lhs.upper - (int64_t) rhs.lower;
  if(!lhs.hasInt32UpperBound | !rhs.hasInt32UpperBound) {
    h = #{noInt32UpperBoundS};
  }

  uint16_t e = math::max(lhs.maxExponent, rhs.maxExponent);
  if(e <= #{maxFiniteExponentS}) {
    e += (uint16_t) 1;
  }

  if(canBeInfiniteOrNan(lhs) & canBeInfiniteOrNan(rhs)) {
    e = #{includesInfinityAndNanS};
  }

  return Range4(l,
                h,
                lhs.canHaveFractionalPart | rhs.canHaveFractionalPart,
                lhs.canBeNegativeZero & canBeZero(rhs),
                e);

}|]

-- One try
-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#805
and :: FunctionDef
and = [funcStr| range and(range lhs, range rhs) {
  if (lhs.lower < (int32_t) 0 & rhs.lower < (int32_t) 0) {
      return newInt32Range(#{int32minS}, math::max(lhs.upper, rhs.upper));
  }

  int32_t lower_ = (int32_t) 0;
  int32_t upper_ = math::min(lhs.upper, rhs.upper);
  if (lhs.lower < (int32_t) 0) {
    upper_ = rhs.upper;
  }
  if (rhs.lower < (int32_t) 0) {
    upper_ = lhs.upper;
  }

  return newInt32Range(lower_, upper_);
}|]

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#834
or :: FunctionDef
or = [funcStr| range or(range lhs, range rhs){
  if (lhs.lower == lhs.upper) {
    if (lhs.lower == (int32_t) 0) {
      return rhs;
    }
    if (lhs.lower == (int32_t) -1) {
      return lhs;
    }
  }

  if (rhs.lower == rhs.upper) {
    if (rhs.lower == (int32_t) 0) {
      return lhs;
    }
    if (rhs.lower == (int32_t) -1) {
      return rhs;
    }
  }

  int32_t lower = #{int32minS};
  int32_t upper = #{int32maxS};
  uint32_t clzLhs = #{uint32minS};
  uint32_t clzRhs = #{uint32minS};
  uint32_t leadingOnes = (uint32_t) 0;

  if(lhs.lower >= (int32_t) 0 & rhs.lower >= (int32_t) 0) {
    lower = math::max(lhs.lower, rhs.lower);
    clzLhs = countLeadingZeroes((uint32_t) lhs.upper);
    clzRhs = countLeadingZeroes((uint32_t) rhs.upper);
    upper = (int32_t) (#{uint32maxS} >> math::min(clzLhs, clzRhs));
  } else {
    if (lhs.upper < (int32_t) 0) {
      leadingOnes = countLeadingZeroes((uint32_t) ~lhs.lower);
      lower = math::max(lower, ~((int32_t) (#{uint32maxS} >> leadingOnes)));
      upper = (int32_t) -1;
    }

    if (rhs.upper < (int32_t) 0) {
      leadingOnes = countLeadingZeroes((uint32_t) ~rhs.lower);
      lower = math::max(lower, ~((int32_t) (#{uint32maxS} >> leadingOnes)));
      upper = (int32_t) -1;
    }
  }

  return newInt32Range(lower, upper);
}|]

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#893
xor :: FunctionDef
xor = [funcStr| range xor(range lhs, range rhs) {
  int32_t lhsLower = lhs.lower;
  int32_t lhsUpper = lhs.upper;

  int32_t rhsLower = rhs.lower;
  int32_t rhsUpper = rhs.upper;

  bool invertAfter = (bool) 0;

  int32_t tmp = (int32_t) 0;
  uint32_t lhsLeadingZeroes = (uint32_t) 0;
  uint32_t rhsLeadingZeroes = (uint32_t) 0;

  if (lhsUpper < (int32_t) 0) {
    lhsLower = ~lhsLower;
    lhsUpper = ~lhsUpper;
    tmp = lhsLower;
    lhsLower = lhsUpper;
    lhsUpper = tmp;
    invertAfter = !invertAfter;
  }

  if (rhsUpper < (int32_t) 0) {
    rhsLower = ~rhsLower;
    rhsUpper = ~rhsUpper;
    tmp = rhsLower;
    rhsLower = rhsUpper;
    rhsUpper = tmp;
    invertAfter = !invertAfter;
  }

  int32_t lower   = #{int32minS};
  int32_t upper   = #{int32maxS};
  int32_t upOr    = #{int32minS};
  int32_t downOr  = #{int32maxS};

  if(lhsLower == (int32_t) 0 & lhsUpper == (int32_t) 0) {
    upper = rhsUpper;
    lower = rhsLower;
  } else if (rhsLower == (int32_t) 0 & rhsUpper == (int32_t) 0) {
    upper = lhsUpper;
    lower = lhsLower;
  } else if (lhsLower >= (int32_t) 0 & rhsLower >= (int32_t) 0) {
    lower = (int32_t) 0;
    lhsLeadingZeroes = countLeadingZeroes((uint32_t) lhsUpper);
    rhsLeadingZeroes = countLeadingZeroes((uint32_t) rhsUpper);

    upOr = rhsUpper | (int32_t) (#{uint32maxS} >> lhsLeadingZeroes);
    downOr = lhsUpper | (int32_t) (#{uint32maxS} >> rhsLeadingZeroes);
    upper = math::min(upOr, downOr);
  }

  if(invertAfter) {
    lower = ~lower;
    upper = ~upper;
    tmp = lower;
    lower = upper;
    upper = tmp;
  }

  return newInt32Range(lower, upper);
}|]
-- Two trys (neg . not)
-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#955
not :: FunctionDef
not = [funcStr| range not(range op) {
    range result_range;
    result_range.lower = ~op.upper;
    result_range.upper = ~op.lower;

    return result_range;
}|]

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#960
mul :: FunctionDef
-- mul =
--   let args = [ ("lhs", c "range")
--              , ("rhs", c "range")
--              ]
--       body = [ declare (t Bool) "newMayIncludeNegativeZero"
--              , declare (t Bool) "sbs"
--              , v "sbs" `assign` (call "canHaveSignBitSet" [v "lhs"])
--              , v "newMayIncludeNegativeZero" `assign` (((call "canHaveSignBitSet" [v "lhs"]) .&&. (call "canBeFiniteNonNegative" [v "rhs"])) .||. ((call "canHaveSignBitSet" [v "rhs"]) .&&. (call "canBeFiniteNonNegative" [v "lhs"])))
--              , declare (t Unsigned16) "exponent"
--              , v "exponent" `assign` n Unsigned16 0
--              , if_ ((not_ $ call "canBeInfiniteOrNan" [v "lhs"]) .&&. (not_ $ call "canBeInfiniteOrNan" [v "rhs"]))
--                [v "exponent" `assign` ((call "numBits" [v "lhs"]) .+. (call "numBits" [v "rhs"]) .-. n Unsigned16 1)
--                , if_ (v "exponent" .>. maxFiniteExponent)
--                  [v "exponent" `assign` includesInfinity] []
--                ]
--                [if_ ((not_ $ call "canBeNan" [v "lhs"]) .&&. (not_ $ call "canBeNan" [v "rhs"]) .&&. (not_ $ (call "canBeZero" [v "lhs"]) .&&. (call "canBeInfiniteOrNan" [v "rhs"])) .&&. (not_ $ (call "canBeZero" [v "rhs"]) .&&. (call "canBeInfiniteOrNan" [v "lhs"]) ))
--                   [v "exponent" `assign` includesInfinity]
--                   [v "exponent" `assign` includesInfinityAndNan]
--                ]
--              , if_ (call "missingAnyInt32Bounds" [v "lhs", v "rhs"])
--                [return_ $ call "Range4" [ noInt32LowerBound
--                                         , noInt32UpperBound
--                                         , (v "lhs" .->. "canHaveFractionalPart") .||. (v "rhs" .->. "canHaveFractionalPart")
--                                         , v "newMayIncludeNegativeZero"
--                                         , v "exponent"
--                                         ]
--                ] []
--              , declare (t Signed64) "a"
--              , declare (t Signed64) "b"
--              , declare (t Signed64) "c"
--              , declare (t Signed64) "d"
--              , v "a" `assign` ((cast (v "lhs" .->. "lower") Signed64) .*. (cast (v "rhs" .->. "lower") Signed64))
--              , v "b" `assign` ((cast (v "lhs" .->. "lower") Signed64) .*. (cast (v "rhs" .->. "upper") Signed64))
--              , v "c" `assign` ((cast (v "lhs" .->. "upper") Signed64) .*. (cast (v "rhs" .->. "lower") Signed64))
--              , v "d" `assign` ((cast (v "lhs" .->. "upper") Signed64) .*. (cast (v "rhs" .->. "upper") Signed64))
--              , return_ $ call "Range4" [ min_ (min_ (v "a") (v "b")) (min_ (v "c") (v "d"))
--                                        , max_ (max_ (v "a") (v "b")) (max_ (v "c") (v "d"))
--                                        , (v "lhs" .->. "canHaveFractionalPart") .||. (v "rhs" .->. "canHaveFractionalPart")
--                                        , v "newMayIncludeNegativeZero"
--                                        , v "exponent"
--                                        ]
--              ]
--   in Function "mul" (c "range") args body

mul = [funcStr| range mul(range lhs, range rhs){
  bool sbs = canHaveSignBitSet(lhs);
  bool newMayIncludeNegativeZero = (bool) 0;
    //(canHaveSignBitSet(lhs) & canBeFiniteNonNegative(rhs)) |
    //(canHaveSignBitSet(rhs) & canBeFiniteNonNegative(lhs));

  uint16_t exponent = (uint16_t) 0;

  if (!canBeInfiniteOrNan(lhs) & !canBeInfiniteOrNan(rhs)) {
    exponent = numBits(lhs) + numBits(rhs) - (uint16_t) 1;

    if (exponent > #{maxFiniteExponentS}) {
      exponent = #{includesInfinityS};
    }

  } else if(!canBeNan(lhs) & !canBeNan(rhs) &
            !(canBeZero(lhs) & canBeInfiniteOrNan(rhs)) &
            !(canBeZero(lhs) & canBeInfiniteOrNan(rhs))) {
    exponent = #{includesInfinityS};
  } else {
    exponent = #{includesInfinityAndNanS};
  }

  if (missingAnyInt32Bounds(lhs, rhs)) {
    return Range4(#{noInt32LowerBoundS},
                  #{noInt32UpperBoundS},
                  lhs.canHaveFractionalPart | rhs.canHaveFractionalPart,
                  newMayIncludeNegativeZero,
                  exponent);
  }

  int64_t a = (int64_t) lhs.lower * (int64_t) rhs.lower;
  int64_t b = (int64_t) lhs.lower * (int64_t) rhs.upper;
  int64_t c = (int64_t) lhs.upper * (int64_t) rhs.lower;
  int64_t d = (int64_t) lhs.upper * (int64_t) rhs.upper;

  return Range4(math::min(math::min(a, b), math::min(c,d)),
                math::max(math::max(a,b), math::max(c,d)),
                lhs . canHaveFractionalPart | rhs.canHaveFractionalPart,
                newMayIncludeNegativeZero,
                exponent);

}|]

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#999
lsh :: FunctionDef
lsh = [funcStr| range lsh(range lhs, int32_t c) {
  int32_t shift = c & (int32_t) 31;
  uint32_t lowerShifted = ((uint32_t) lhs.lower << shift << (uint32_t) 1) >> shift >> (int32_t) 1;
  uint32_t upperShifted = ((uint32_t) lhs.upper << shift << (uint32_t) 1) >> shift >> (int32_t) 1;

  bool canShift = (int32_t) lowerShifted == lhs.lower & (int32_t) upperShifted == lhs.upper;

  if(canShift) {
    return newInt32Range((int32_t) ((uint32_t) lhs.lower << shift),
                         (int32_t) ((uint32_t) lhs.upper << shift));
  }

  return newInt32Range(#{int32minS}, #{int32maxS});
}|]

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1016
rsh :: FunctionDef
rsh = [funcStr| range rsh(range lhs, int32_t c) {
  int32_t shift = c & (int32_t) 31;
  return newInt32Range(lhs.lower >> shift,
                       lhs.upper >> shift);
}|]


-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1023
ursh :: FunctionDef
ursh = [funcStr| range ursh(range lhs, int32_t c) {
  int32_t shift = c & (int32_t) 31;
  if (isFiniteNonNegative(lhs) | isFiniteNegative(lhs)) {
    return newUInt32Range((uint32_t)(lhs.lower) >> shift,
                          (uint32_t)(lhs.upper) >> shift);
  }
  return newUInt32Range((uint32_t) 0, #{uint32maxS} >> shift);
}|]

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1042
lsh' :: FunctionDef
lsh' = [funcStr| range lsh'(range lhs, range rhs) {
  return newInt32Range(#{int32minS}, #{int32maxS});
}|]

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1048
rsh' :: FunctionDef
rsh' = [funcStr| range rsh'(range lhs, range rhs) {
  int32_t shiftLower = rhs.lower;
  int32_t shiftUpper = rhs.upper;
  if ((int64_t) shiftUpper - (int64_t) shiftLower >= (int64_t) 31) {
    shiftLower = (int32_t) 0;
    shiftUpper = (int32_t) 31;
  } else {
    shiftLower &= (int32_t) 31;
    shiftUpper &= (int32_t) 31;
    if (shiftLower > shiftUpper) {
      shiftLower = (int32_t) 0;
      shiftUpper = (int32_t) 31;
    }
  }

  int32_t lhsLower = lhs.lower;
  int32_t min = lhsLower < (int32_t) 0 ?
                lhsLower >> shiftLower :
                lhsLower >> shiftUpper;
  int32_t lhsUpper = lhs.upper;
  int32_t max = lhsUpper >= (int32_t) 0 ?
                lhsUpper >> shiftLower :
                lhsUpper >> shiftUpper;

  return newInt32Range(min, max);
}|]

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1079
ursh' :: FunctionDef
ursh' = [funcStr| range ursh'(range lhs, range rhs) {
  return newUInt32Range((uint32_t) 0, isFiniteNonNegative(lhs) ?  (uint32_t) lhs.upper : #{uint32maxS});
}|]

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1089
abs :: FunctionDef
abs = [funcStr| range abs(range op) {
  int32_t l = op.lower;
  int32_t u = op.upper;
  return Range6(
    (int64_t) math::max(math::max((int32_t) 0, l), u == #{int32minS} ? #{int32maxS} : -u),
    (bool) 1,
    (int64_t) math::max(math::max((int32_t) 0, u), l == #{int32minS} ? #{int32maxS} : -l),
    hasInt32Bounds(op) & l != #{int32minS},
    op.canHaveFractionalPart,
    #{excludesNegativeZeroS},
    op.maxExponent
  );
}|]

-- Nan thing
-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1104
min :: FunctionDef
min = [funcStr| range min(range lhs, range rhs) {
  return Range6(
    (int64_t) math::min(lhs.lower, rhs.lower),
    lhs.hasInt32LowerBound & rhs.hasInt32LowerBound,
    (int64_t) math::min(lhs.upper, rhs.upper),
    lhs.hasInt32UpperBound | rhs.hasInt32UpperBound,
    lhs.canHaveFractionalPart | rhs.canHaveFractionalPart,
    lhs.canBeNegativeZero | rhs.canBeNegativeZero,
    math::max(lhs.maxExponent, rhs.maxExponent)
  );
}|]

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1123
max :: FunctionDef
max = [funcStr| range max(range lhs, range rhs){
  return Range6(
    (int64_t) math::max(lhs.lower, rhs.lower),
    lhs.hasInt32LowerBound | rhs.hasInt32LowerBound,
    (int64_t) math::max(lhs.upper, rhs.upper),
    lhs.hasInt32UpperBound & rhs.hasInt32UpperBound,
    lhs.canHaveFractionalPart | rhs.canHaveFractionalPart,
    lhs.canBeNegativeZero | rhs.canBeNegativeZero,
    math::max(lhs.maxExponent, rhs.maxExponent)
  );
}|]

-- Add fractional part
-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1142
floor :: FunctionDef
floor = [funcStr| range floor(range op) {
  range copy = op;
  range tmp = op;

  if(op.canHaveFractionalPart & op.hasInt32LowerBound) {
    copy = setLowerInit((int64_t) copy.lower - (int64_t) 1, tmp);
  }

  if (hasInt32Bounds(copy)) {
    copy.maxExponent = exponentImpliedByInt32Bounds(copy);
  } else if (copy.maxExponent < #{maxFiniteExponentS}) {
    copy.maxExponent += (uint16_t) 1;
  }

  copy.canHaveFractionalPart = #{excludesFractionalPartsS};
  return copy;
}|]

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1166
ceil :: FunctionDef
ceil = [funcStr| range ceil(range op) {
  range copy = op;

  // missing fract check

  if (hasInt32Bounds(copy)) {
    copy.maxExponent = exponentImpliedByInt32Bounds(copy);
  } else if (copy.maxExponent < #{maxFiniteExponentS}) {
    copy.maxExponent += (uint16_t) 1;
  }

  copy.canHaveFractionalPart = #{excludesFractionalPartsS};
  return copy;
}|]
-- Not doing nan thing
-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1184
sign :: FunctionDef
-- sign =
--   let args = [ ("op", c "range") ]
--       body = [ return_ $ call "Range4" [ cast (max_ ( min_ (v "op" .->. "lower") (n Signed 1) ) (n Signed (-1))) Signed64
--                                        , cast (max_ ( min_ (v "op" .->. "upper") (n Signed 1) ) (n Signed (-1))) Signed64
--                                        , excludesFractionalParts
--                                        , v "op" .->. "canBeNegativeZero"
--                                        , n Unsigned16 0
--                                        ]
--              ]
--   in Function "sign" (c "range") args body
sign = [funcStr| range sign(range op) {
  return Range4(
    (int64_t) math::max(math::min(op.lower, (int32_t) 1), (int32_t) -1),
    (int64_t) math::max(math::min(op.upper, (int32_t) 1), (int32_t) -1),
    #{excludesFractionalPartsS},
    op.canBeNegativeZero,
    (uint16_t) 0
  );
}|]

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#485
intersect :: FunctionDef
intersect =
  let args = [ ("lhs", c "range")
             , ("rhs", c "range")
             ]
      body = [ declare (t Signed) "newLower"
             , declare (t Signed) "newUpper"
             , declare (t Bool) "emptyRange"
             , declare (t Bool) "welter"
             , v "emptyRange" `assign` n Bool 0
             , v "newLower" `assign` max_ (v "lhs" .->. "lower") (v "rhs" .->. "lower")
             , v "newUpper" `assign` min_ (v "lhs" .->. "upper") (v "rhs" .->. "upper")
             , if_ (v "newUpper" .<. v "newLower")
               [ if_ ((not_ $ call "canBeNan" [v "lhs"]) .&&. (not_ $ call "canBeNan" [v "rhs"]))
                 [v "emptyRange" `assign` n Bool 1] []
               , return_ $ call "nullRange" [v "emptyRange"]
               ]  []
             , declare (t Bool) "newHasInt32LowerBound"
             , declare (t Bool) "newHasInt32UpperBound"
             , declare (t Bool) "newMayIncludeNegativeZero"
             , declare (t Bool) "newCanHaveFractionalPart"
             , declare (t Unsigned16) "newExponent"

             , v "newHasInt32LowerBound" `assign` ((v "lhs" .->. "hasInt32LowerBound") .||. (v "rhs" .->. "hasInt32LowerBound"))
             , v "newHasInt32UpperBound" `assign` ((v "lhs" .->. "hasInt32UpperBound") .||. (v "rhs" .->. "hasInt32UpperBound"))

             , v "newCanHaveFractionalPart" `assign` ((v "lhs" .->. "canHaveFractionalPart") .&&. (v "rhs" .->. "canHaveFractionalPart"))

             , v "newMayIncludeNegativeZero" `assign` ( (v "lhs" .->. "canBeNegativeZero") .&&. (v "lhs" .->. "canBeNegativeZero") )

             , v "newExponent" `assign` min_ (v "lhs" .->. "maxExponent") (v "rhs" .->. "maxExponent")


             , if_ (v "newHasInt32LowerBound" .&&. v "newHasInt32UpperBound" .&&. (v "newExponent" .==. includesInfinityAndNan)) [return_ $ call "nullRange" [v "emptyRange"]] []

             , declare (c "range") "intersect_result"
             , v "intersect_result" .->. "lower" `assign` v "newLower"
             , v "intersect_result" .->. "hasInt32LowerBound" `assign` v "newHasInt32LowerBound"
             , v "intersect_result" .->. "upper" `assign` v "newUpper"
             , v "intersect_result" .->. "hasInt32UpperBound" `assign` v "newHasInt32UpperBound"
             , v "intersect_result" .->. "canBeNegativeZero" `assign` v "newMayIncludeNegativeZero"
             , v "intersect_result" .->. "canHaveFractionalPart" `assign` v "newCanHaveFractionalPart"
             , v "intersect_result" .->. "maxExponent" `assign` v "newExponent"
             , v "intersect_result" .->. "isEmpty" `assign` v "emptyRange"
             , return_ $ v "intersect_result"
             ]
   in Function "intersect" (c "range") args body

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#485
brokenIntersect :: FunctionDef
brokenIntersect =
  let args = [ ("lhs", c "range")
             , ("rhs", c "range")
             ]
      body = [ declare (t Signed) "newLower"
             , declare (t Signed) "newUpper"
             , declare (t Bool) "emptyRange"
             , v "emptyRange" `assign` n Bool 0
             , v "newLower" `assign` max_ (v "lhs" .->. "lower") (v "rhs" .->. "lower")
             , v "newUpper" `assign` min_ (v "lhs" .->. "upper") (v "rhs" .->. "upper")
             , if_ (v "newUpper" .<. v "newLower")
               [ v "emptyRange" `assign` n Bool 1
               , return_ $ call "nullRange" [v "emptyRange"]
               ]  []
             , declare (t Bool) "newHasInt32LowerBound"
             , declare (t Bool) "newHasInt32UpperBound"
             , declare (t Bool) "newMayIncludeNegativeZero"
             , declare (t Bool) "newCanHaveFractionalPart"
             , declare (t Unsigned16) "newExponent"

             , v "newHasInt32LowerBound" `assign` ((v "lhs" .->. "hasInt32LowerBound") .||. (v "rhs" .->. "hasInt32LowerBound"))
             , v "newHasInt32UpperBound" `assign` ((v "lhs" .->. "hasInt32UpperBound") .||. (v "rhs" .->. "hasInt32UpperBound"))

             , v "newCanHaveFractionalPart" `assign` ((v "lhs" .->. "canHaveFractionalPart") .&&. (v "rhs" .->. "canHaveFractionalPart"))
             , v "newMayIncludeNegativeZero" `assign` ( (v "lhs" .->. "canBeNegativeZero") .&&. (v "lhs" .->. "canBeNegativeZero") )

             , v "newExponent" `assign` min_ (v "lhs" .->. "maxExponent") (v "rhs" .->. "maxExponent")

             , if_ (v "newHasInt32LowerBound" .&&. v "newHasInt32UpperBound" .&&. (v "newExponent" .==. includesInfinityAndNan)) [return_ $ call "nullRange" [v "emptyRange"]] []

             , declare (c "range") "intersect_result"
             , v "intersect_result" .->. "lower" `assign` v "newLower"
             , v "intersect_result" .->. "hasInt32LowerBound" `assign` v "newHasInt32LowerBound"
             , v "intersect_result" .->. "upper" `assign` v "newUpper"
             , v "intersect_result" .->. "hasInt32UpperBound" `assign` v "newHasInt32UpperBound"
             , v "intersect_result" .->. "canBeNegativeZero" `assign` v "newMayIncludeNegativeZero"
             , v "intersect_result" .->. "canHaveFractionalPart" `assign` v "newCanHaveFractionalPart"
             , v "intersect_result" .->. "maxExponent" `assign` v "newExponent"
             , v "intersect_result" .->. "isEmpty" `assign` v "emptyRange"
             , return_ $ v "intersect_result"
             ]
   in Function "intersect" (c "range") args body


-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#579
union :: FunctionDef
union = [funcStr| range union(range& lhs, range& rhs){
  int32_t newLower = math::min(lhs.lower, rhs.lower);
  int32_t newUpper = math::max(lhs.upper, rhs.upper);
  bool newHasInt32LowerBound = lhs.hasInt32LowerBound & rhs.hasInt32LowerBound;
  bool newHasInt32UpperBound = lhs.hasInt32UpperBound & rhs.hasInt32UpperBound;
  bool newCanHaveFractionalPart = lhs.canHaveFractionalPart | rhs.canHaveFractionalPart;
  bool newMayIncludeNegativeZero = lhs.canBeNegativeZero |rhs.canBeNegativeZero;
  uint16_t newExponent = math::max(lhs.maxExponent, rhs.maxExponent);

  range unionRet;
  unionRet.lower = newLower;
  unionRet.hasInt32LowerBound = newHasInt32LowerBound;
  unionRet.upper = newUpper;
  unionRet.hasInt32UpperBound = newHasInt32UpperBound;
  unionRet.canHaveFractionalPart = newCanHaveFractionalPart;
  unionRet.canBeNegativeZero = newMayIncludeNegativeZero;
  unionRet.maxExponent = newExponent;
  unionRet.isEmpty = (bool) 0;
  return unionRet;
}|]
