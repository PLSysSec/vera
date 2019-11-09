range add(range lhs, range rhs) {
int64_t l;

int64_t h;

uint16_t e;

l = ((int64_t)(lhs_lower)) + ((int64_t)(rhs_lower));

if((!(lhs_hasInt32LowerBound)) | (!(rhs_hasInt32LowerBound))) {
l = ((int64_t)(2147483648)) - (1);

}

h = ((int64_t)(lhs_upper)) + ((int64_t)(rhs_upper));

if((!(lhs_hasInt32UpperBound)) | (!(rhs_hasInt32UpperBound))) {
h = ((int64_t)(2147483647)) - (1);

}

e = max((lhs_maxExponent), (rhs_maxExponent));

if((e) < (1023)) {
e += 1;

}

if((canBeInfiniteOrNan(lhs)) & (canBeInfiniteOrNan(rhs))) {
e = 65535;

}

return Range3(l, h, (lhs_canBeNegativeZero) & (rhs_canBeNegativeZero));

}
