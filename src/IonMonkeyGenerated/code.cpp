#define excludesFractionalPartsS ((bool) 0)
#define int32minS ((int32_t) -2147483648)
#define int32maxS ((int32_t) 2147483647)
#define uint32maxS ((uint32_t) 4294967295)
#define uint32minS ((uint32_t) 0)
#define excludesNegativeZeroS ((bool) 0)
#define maxFiniteExponentS ((uint16_t) 1023)
#define includesInfinityS ((uint16_t) 1 + maxFiniteExponentS)
#define includesInfinityAndNanS ((uint16_t) 65535)
#define noInt32LowerBoundS ((int64_t) jsIntMinS - (int64_t) 1)
#define noInt32UpperBoundS ((int64_t) jsIntMaxS + (int64_t) 1)
#define jsIntMaxS ((int32_t) 0x7fffffff)
#define jsIntMinS ((int32_t) 0x80000000)
#define jsIntMin64S ((int64_t) -2147483648)
#define jsIntMax64S ((int64_t) 2147483647)
#define maxInt32ExponentS ((uint16_t) 31)
#define maxUInt32ExponentS ((uint16_t) 31)

struct range {
    int32_t lower;
    int32_t upper;
    bool hasInt32LowerBound;
    bool hasInt32UpperBound;
    bool canHaveFractionalPart;
    bool canBeNegativeZero;
    uint16_t maxExponent;
    bool isEmpty;
};

// -------------------
// Helpers
// -------------------
range nullRange(bool emptyR) {
  range nrRet;
  nrRet.lower = jsIntMinS;
  nrRet.hasInt32UpperBound = (bool) 0;
  nrRet.upper = jsIntMaxS;
  nrRet.hasInt32LowerBound = (bool) 0;
  nrRet.canHaveFractionalPart = (bool) 1;
  nrRet.canBeNegativeZero = (bool) 1;
  nrRet.maxExponent = includesInfinityAndNanS;
  nrRet.isEmpty = emptyR;
  return nrRet;
}

// https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#394
range newInt32Range(int32_t lower_bound_vv, int32_t upper_bound_vv) {
   range rvvv;
   rvvv.lower = lower_bound_vv;
   rvvv.upper = upper_bound_vv;
   rvvv.hasInt32UpperBound = (bool) 1;
   rvvv.hasInt32LowerBound = (bool) 1;
   rvvv.canHaveFractionalPart = excludesFractionalPartsS;
   rvvv.canBeNegativeZero = excludesNegativeZeroS;
   rvvv.maxExponent = maxInt32ExponentS;
   rvvv.isEmpty = (bool) 0;
   return rvvv;
}

range newUInt32Range(uint32_t u_lower_bound, uint32_t u_upper_bound) {
   return Range4((int64_t) u_lower_bound, (int64_t) u_upper_bound,
                  excludesFractionalPartsS, excludesNegativeZeroS, maxUInt32ExponentS);
}

bool canBeFiniteNonNegative(range const& fnn2) {
  return fnn2.upper >= (int32_t) 0;
}

bool canBeFiniteNegative(range const& fnn3) {
  return fnn3.lower < (int32_t) 0;
}

bool canBeNan(range const& nannan) {
  return nannan.maxExponent == includesInfinityAndNanS;
}

bool contains(range const& crange, int32_t cval) {
  return cval >= crange.lower & cval <= crange.upper;
}

bool canBeZero(range const& zrange) {
  return contains(zrange, (int32_t)0);
}

uint32_t countOnes(uint32_t y) {
  y -= (y >> (int32_t)1) & (uint32_t) 1431655765;
  y = ((y >> (uint32_t)2) & (uint32_t)858993459) + (y & (uint32_t)858993459);
  y = ((y >> (uint32_t)4) + y) & (uint32_t)252645135;
  y += y >> (uint32_t)8;
  y += y >> (uint32_t)16;
  return y & (uint32_t)63;
}

uint32_t countLeadingZeroes(uint32_t x) {
  x |= x >> (uint32_t)1;
  x |= x >> (uint32_t)2;
  x |= x >> (uint32_t)4;
  x |= x >> (uint32_t)8;
  x |= x >> (uint32_t)16;

  uint32_t ones = countOnes(x);
  return (uint32_t)32 - ones;
}

// https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#566
bool canHaveSignBitSet(range const& csbs_range) {
  return (!csbs_range.hasInt32LowerBound) | canBeFiniteNegative(csbs_range) | csbs_range.canBeNegativeZero;
}

// https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#566
uint16_t exponentImpliedByInt32Bounds(range const& eib_range) {
  int32_t ua = eib_range.upper;
  int32_t la = eib_range.lower;
  int32_t abs_upper = math::abs(ua);
  int32_t abs_lower = math::abs(la);
  int32_t themax = (int32_t) math::max((uint32_t) abs_lower, (uint32_t) abs_upper);
  uint16_t eib_ret = math::exp((double) themax);
  return eib_ret;
}

range setLowerInit(int64_t sli_x, range sli_range) {

  if (sli_x > jsIntMax64S) {
    sli_range.lower = jsIntMaxS;
    sli_range.hasInt32LowerBound = (bool)1;
  } else if(sli_x < jsIntMin64S) {
    sli_range.lower = jsIntMinS;
    sli_range.hasInt32LowerBound = (bool)0;
  } else {
    sli_range.lower = (int32_t)sli_x;
    sli_range.hasInt32LowerBound = (bool)1;
  }

  return sli_range;
}


range Range3(int64_t lower_bound, int64_t upper_bound, bool nz_flag) {
   range rv;
   range tmp = rv;
   rv = setLowerInit(lower_bound, tmp);

   range tmp2;
   tmp2 = rv;
   rv = setUpperInit(upper_bound, tmp2);
   rv.canBeNegativeZero = nz_flag;
   return rv;
}

range Range4(
  int64_t lower_bound,
  int64_t upper_bound,
  bool fract_flag,
  bool nz_flag,
  uint16_t exp_set) {

  range rv;
  range tmp = rv;
  rv = setLowerInit(lower_bound, tmp);

  range tmp2 = rv;
  rv = setUpperInit(upper_bound, tmp2);
  rv.canHaveFractionalPart = fract_flag;
  rv.canBeNegativeZero = nz_flag;
  rv.maxExponent = exp_set;
  rv.isEmpty = (bool) 0;
  return rv;
}

range Range6(int64_t lower_bound, bool has_lower,
             int64_t upper_bound, bool has_upper,
             bool fract_flag, bool nz_flag, uint16_t exp_set) {
  range rv;
  range tmp = rv;

  rv = setLowerInit(lower_bound, tmp);

  rv.hasInt32LowerBound = has_lower;

  range tmp2 = rv;
  rv = setUpperInit(upper_bound, tmp2);
  rv.hasInt32UpperBound = has_upper;
  rv.canHaveFractionalPart = fract_flag;
  rv.canBeNegativeZero = nz_flag;
  rv.maxExponent = exp_set;
  rv.isEmpty = (bool) 0;

  return rv;
}

range setUpperInit(int64_t sui_x, range sui_range) {
   if (sui_x > jsIntMax64S) {
    sui_range.upper = jsIntMaxS;
    sui_range.hasInt32UpperBound = (bool) 0;
   } else {
      if (sui_x < jsIntMin64S) {
        sui_range.upper = jsIntMinS;
        sui_range.hasInt32UpperBound = (bool) 1;
      } else  {
        sui_range.upper = (int32_t) sui_x;
        sui_range.hasInt32UpperBound = (bool) 1;
      }
   }
   return sui_range;
}

bool hasInt32Bounds(range const& bnds) {
  return bnds.hasInt32LowerBound & bnds.hasInt32UpperBound;
}

uint16_t numBits(range const& nbs) {
 return nbs.maxExponent + ((uint16_t) 1);
}

bool isFiniteNonNegative(range const& fnn) {
  return fnn.lower > ((int32_t) 0);
}

bool isFiniteNegative(range const& fn) {
  return fn.upper < ((int32_t) 0);
}

bool canBeInfiniteOrNan(range const& fnan) {
  return fnan.maxExponent >= includesInfinityS;
}

bool missingAnyInt32Bounds(range const& mibs1, range const& mibs2) {
  return (!hasInt32Bounds(mibs1)) | (!hasInt32Bounds(mibs2));
}

// -------------------
// Operations
// -------------------
range add(range const& lhs, range const& rhs)
{
    int64_t l;
    int64_t h;
    uint16_t e;

    l = (int64_t)lhs.lower + (int64_t)rhs.lower;
    if (!lhs.hasInt32LowerBound | !rhs.hasInt32LowerBound)
    {
        l = noInt32LowerBoundS;
    }

    h = (int64_t)lhs.upper + (int64_t)rhs.upper;
    if (!lhs.hasInt32UpperBound | !rhs.hasInt32UpperBound)
    {
        h = noInt32UpperBoundS;
    }

    e = math::max(lhs.maxExponent, rhs.maxExponent);
    if (e <= maxFiniteExponentS)
    {
        e += (uint16_t)1;
    }

    if (canBeInfiniteOrNan(lhs) & canBeInfiniteOrNan(rhs))
    {
        e = includesInfinityAndNanS;
    }

    return Range4(l,
                  h,
                  lhs.canHaveFractionalPart | rhs.canHaveFractionalPart,
                  lhs.canBeNegativeZero & rhs.canBeNegativeZero,
                  e);

}

range sub(range const& lhs, range const& rhs) {

  int64_t l = (int64_t) lhs.lower - (int64_t) rhs.upper;
  if(!lhs.hasInt32LowerBound | !rhs.hasInt32UpperBound) {
    l = noInt32LowerBoundS;
  }

  int64_t h = (int64_t) lhs.upper - (int64_t) rhs.lower;
  if(!lhs.hasInt32UpperBound | !rhs.hasInt32LowerBound) {
    h = noInt32UpperBoundS;
  }

  uint16_t e = math::max(lhs.maxExponent, rhs.maxExponent);
  if(e <= maxFiniteExponentS) {
    e += (uint16_t) 1;
  }

  if(canBeInfiniteOrNan(lhs) & canBeInfiniteOrNan(rhs)) {
    e = includesInfinityAndNanS;
  }

  return Range4(l,
                h,
                lhs.canHaveFractionalPart | rhs.canHaveFractionalPart,
                lhs.canBeNegativeZero & canBeZero(rhs),
                e);

}


range and_(range const& lhs, range const& rhs) {
  if (lhs.lower < (int32_t) 0 & rhs.lower < (int32_t) 0) {
      return newInt32Range(int32minS, math::max(lhs.upper, rhs.upper));
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
}

range or_(range const& lhs, range const& rhs){
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

  int32_t lower = int32minS;
  int32_t upper = int32maxS;
  uint32_t clzLhs = uint32minS;
  uint32_t clzRhs = uint32minS;
  uint32_t leadingOnes = (uint32_t) 0;

  if(lhs.lower >= (int32_t) 0 & rhs.lower >= (int32_t) 0) {
    lower = math::max(lhs.lower, rhs.lower);
    clzLhs = countLeadingZeroes((uint32_t) lhs.upper);
    clzRhs = countLeadingZeroes((uint32_t) rhs.upper);
    upper = (int32_t) (uint32maxS >> math::min(clzLhs, clzRhs));
  } else {
    if (lhs.upper < (int32_t) 0) {
      leadingOnes = countLeadingZeroes((uint32_t) ~lhs.lower);
      lower = math::max(lower, ~((int32_t) (uint32maxS >> leadingOnes)));
      upper = (int32_t) -1;
    }

    if (rhs.upper < (int32_t) 0) {
      leadingOnes = countLeadingZeroes((uint32_t) ~rhs.lower);
      lower = math::max(lower, ~((int32_t) (uint32maxS >> leadingOnes)));
      upper = (int32_t) -1;
    }
  }

  return newInt32Range(lower, upper);
}

range xor_(range const& lhs, range const& rhs) {
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

  int32_t lower   = int32minS;
  int32_t upper   = int32maxS;
  int32_t upOr    = int32minS;
  int32_t downOr  = int32maxS;

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

    upOr = rhsUpper | (int32_t) (uint32maxS >> lhsLeadingZeroes);
    downOr = lhsUpper | (int32_t) (uint32maxS >> rhsLeadingZeroes);
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
}

range not_(range const& op) {
    return newInt32Range(~op.upper, ~op.lower);
}

range mul(range const& lhs, range const& rhs){
  bool newMayIncludeNegativeZero = 
    (canHaveSignBitSet(lhs) & canBeFiniteNonNegative(rhs)) |
    (canHaveSignBitSet(rhs) & canBeFiniteNonNegative(lhs));

  uint16_t exponent = (uint16_t) 0;

  if (!canBeInfiniteOrNan(lhs) & !canBeInfiniteOrNan(rhs)) {
    exponent = numBits(lhs) + numBits(rhs) - (uint16_t) 1;

    if (exponent > maxFiniteExponentS) {
      exponent = includesInfinityS;
    }

  } else if(!canBeNan(lhs) & !canBeNan(rhs) &
            !(canBeZero(lhs) & canBeInfiniteOrNan(rhs)) &
            !(canBeZero(rhs) & canBeInfiniteOrNan(lhs))) {
    exponent = includesInfinityS;
  } else {
    exponent = includesInfinityAndNanS;
  }

  if (missingAnyInt32Bounds(lhs, rhs)) {
    return Range4(noInt32LowerBoundS,
                  noInt32UpperBoundS,
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

}

// https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#999
range lsh(range const& lhs, int32_t c) {
  int32_t shift = c & (int32_t) 31;
  uint32_t lowerShifted = ((uint32_t) lhs.lower << shift << (uint32_t) 1) >> shift >> (int32_t) 1;
  uint32_t upperShifted = ((uint32_t) lhs.upper << shift << (uint32_t) 1) >> shift >> (int32_t) 1;

  bool canShift = (int32_t) lowerShifted == lhs.lower & (int32_t) upperShifted == lhs.upper;

  if(canShift) {
    return newInt32Range((int32_t) ((uint32_t) lhs.lower << shift),
                         (int32_t) ((uint32_t) lhs.upper << shift));
  }

  return newInt32Range(int32minS, int32maxS);
}

// https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1016
range rsh(range const& lhs, int32_t c) {
  int32_t shift = c & (int32_t) 31;
  return newInt32Range(lhs.lower >> shift,
                       lhs.upper >> shift);
}

// https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1023
range ursh(range const& lhs, int32_t c) {
  int32_t shift = c & (int32_t) 31;
  if (isFiniteNonNegative(lhs) | isFiniteNegative(lhs)) {
    return newUInt32Range((uint32_t)(lhs.lower) >> shift,
                          (uint32_t)(lhs.upper) >> shift);
  }
  return newUInt32Range((uint32_t) 0, uint32maxS >> shift);
}

// https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1042
range lsh_p(range const& lhs, range const& rhs) {
  return newInt32Range(int32minS, int32maxS);
}

// https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1048
range rsh_p(range const& lhs, range const& rhs) {
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
}

// https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1079
range ursh_p(range const& lhs, range const& rhs) {
  return newUInt32Range((uint32_t) 0, isFiniteNonNegative(lhs) ?  (uint32_t) lhs.upper : uint32maxS);
}

// https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1089
range abs(range const& op) {
  int32_t l = op.lower;
  int32_t u = op.upper;
  return Range6(
    (int64_t) math::max(math::max((int32_t) 0, l), u == int32minS ? int32maxS : -u),
    (bool) 1,
    (int64_t) math::max(math::max((int32_t) 0, u), l == int32minS ? int32maxS : -l),
    hasInt32Bounds(op) & (l != int32minS),
    op.canHaveFractionalPart,
    excludesNegativeZeroS,
    op.maxExponent
  );
}

// https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1104
range min(range const& lhs, range const& rhs) {
  return Range6(
    (int64_t) math::min(lhs.lower, rhs.lower),
    lhs.hasInt32LowerBound & rhs.hasInt32LowerBound,
    (int64_t) math::min(lhs.upper, rhs.upper),
    lhs.hasInt32UpperBound | rhs.hasInt32UpperBound,
    lhs.canHaveFractionalPart | rhs.canHaveFractionalPart,
    lhs.canBeNegativeZero | rhs.canBeNegativeZero,
    math::max(lhs.maxExponent, rhs.maxExponent)
  );
}

// https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1123
range max(range const& lhs, range const& rhs){
  return Range6(
    (int64_t) math::max(lhs.lower, rhs.lower),
    lhs.hasInt32LowerBound | rhs.hasInt32LowerBound,
    (int64_t) math::max(lhs.upper, rhs.upper),
    lhs.hasInt32UpperBound & rhs.hasInt32UpperBound,
    lhs.canHaveFractionalPart | rhs.canHaveFractionalPart,
    lhs.canBeNegativeZero | rhs.canBeNegativeZero,
    math::max(lhs.maxExponent, rhs.maxExponent)
  );
}

// https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1142
range floor(range const& op) {
  range copy = op;
  range tmp = op;

  if(op.canHaveFractionalPart & op.hasInt32LowerBound) {
    copy = setLowerInit((int64_t) copy.lower - (int64_t) 1, tmp);
  }

  if (hasInt32Bounds(copy)) {
    copy.maxExponent = exponentImpliedByInt32Bounds(copy);
  } else if (copy.maxExponent < maxFiniteExponentS) {
    copy.maxExponent += (uint16_t) 1;
  }

  copy.canHaveFractionalPart = excludesFractionalPartsS;
  return copy;
}

// https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1166
range ceil(range const& op) {
  range copy = op;

  // missing fract check

  if (hasInt32Bounds(copy)) {
    copy.maxExponent = exponentImpliedByInt32Bounds(copy);
  } else if (copy.maxExponent < maxFiniteExponentS) {
    copy.maxExponent += (uint16_t) 1;
  }

  copy.canHaveFractionalPart = excludesFractionalPartsS;
  return copy;
}
// https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1184
range sign(range const& op) {

  if (canBeNan(op)) {
    return nullRange((bool) 1);
  }

  return Range4(
    (int64_t) math::max(math::min(op.lower, (int32_t) 1), (int32_t) -1),
    (int64_t) math::max(math::min(op.upper, (int32_t) 1), (int32_t) -1),
    excludesFractionalPartsS,
    op.canBeNegativeZero,
    (uint16_t) 0
  );
}

// https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#485
range intersect(range const& lhs, range const& rhs){
   int32_t newLower = math::max(lhs.lower, rhs.lower);
   int32_t newUpper = math::min(lhs.upper, rhs.upper);
   bool  emptyRange = (bool)0;

   if (newUpper < newLower) {
     if (!canBeNan(lhs) | !canBeNan(rhs)) {
       emptyRange = (bool)1;
     }
     return nullRange(emptyRange);
   }
   bool newHasInt32LowerBound = lhs.hasInt32LowerBound | rhs.hasInt32LowerBound;
   bool newHasInt32UpperBound = lhs.hasInt32UpperBound | rhs.hasInt32UpperBound;
   bool newMayIncludeNegativeZero = lhs.canBeNegativeZero & rhs.canBeNegativeZero;
   bool newCanHaveFractionalPart = lhs.canHaveFractionalPart & rhs.canHaveFractionalPart;
   uint16_t newExponent = math::min(lhs.maxExponent, rhs.maxExponent);

   if (newHasInt32LowerBound & newHasInt32UpperBound & (newExponent == includesInfinityAndNanS)) {
       return nullRange(emptyRange);
   }

   range intersect_result;
   intersect_result.lower = newLower;
   intersect_result.hasInt32LowerBound = newHasInt32LowerBound;
   intersect_result.upper = newUpper;
   intersect_result.hasInt32UpperBound = newHasInt32UpperBound;
   intersect_result.canBeNegativeZero = newMayIncludeNegativeZero;
   intersect_result.canHaveFractionalPart = newCanHaveFractionalPart;
   intersect_result.maxExponent = newExponent;
   intersect_result.isEmpty = emptyRange;
   return intersect_result;
}

// https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#579
range union_(range const& lhs, range const& rhs){
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
}
