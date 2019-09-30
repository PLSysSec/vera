module V8.Operations where
import qualified DSL.DSL           as D
import           IonMonkey.Objects
import           Prelude           hiding (max, min)
import           V8.Helpers

-- | https://cs.chromium.org/chromium/src/v8/src/compiler/operation-typer.cc?q=NumberAdd&sq=package:chromium&g=0&l=589
numberAdd :: Range -> Range -> D.Verif Range
numberAdd = undefined

-- | https://cs.chromium.org/chromium/src/v8/src/compiler/operation-typer.cc?q=NumberAdd&sq=package:chromium&g=0&l=634
numberSubtract :: Range -> Range -> D.Verif Range
numberSubtract = undefined

-- | https://cs.chromium.org/chromium/src/v8/src/compiler/operation-typer.cc?q=NumberAdd&sq=package:chromium&g=0&l=697
numberMultiply :: Range -> Range -> D.Verif Range
numberMultiply = undefined

-- | https://cs.chromium.org/chromium/src/v8/src/compiler/operation-typer.cc?q=NumberAdd&sq=package:chromium&g=0&l=742
numberDivide :: Range -> Range -> D.Verif Range
numberDivide = undefined

-- | https://cs.chromium.org/chromium/src/v8/src/compiler/operation-typer.cc?q=NumberAdd&sq=package:chromium&g=0&l=771
numberModulus :: Range -> Range -> D.Verif Range
numberModulus = undefined

-- | https://cs.chromium.org/chromium/src/v8/src/compiler/operation-typer.cc?q=NumberAdd&sq=package:chromium&g=0&l=840
numberBitwiseOr :: Range -> Range -> D.Verif Range
numberBitwiseOr = undefined

-- | https://cs.chromium.org/chromium/src/v8/src/compiler/operation-typer.cc?q=NumberAdd&sq=package:chromium&g=0&l=877
numberBitwiseAnd :: Range -> Range -> D.Verif Range
numberBitwiseAnd left right = do
  zero <- D.i32c 0

  leftNotNeg <- D.sgte (lower left) zero
  rightNotNeg <- D.sgte (lower right) zero
  min <- D.i32min

  max1 <- do
    cond <- D.and leftNotNeg rightNotNeg
    trueBr <- D.smin (upper left) (upper right)
    falseBr <- D.smax (upper left) (upper right)
    D.cond cond trueBr falseBr

  max2 <- do
    calc <- D.smin max1 (upper left)
    D.cond leftNotNeg calc max1

  max3 <- do
    calc <- D.smin max1 (upper right)
    D.cond rightNotNeg calc max1

  result <- newResultRange "result" D.i32
  D.assign (lower result) min
  D.assign (upper result) max3
  return result

-- | https://cs.chromium.org/chromium/src/v8/src/compiler/operation-typer.cc?q=NumberAdd&sq=package:chromium&g=0&l=908
numberBitwiseXor :: Range -> Range -> D.Verif Range
numberBitwiseXor left right = do
  zero <- D.i32c 0

  -- First conditional: return Unsigned31
  leftNonNeg <- D.sgte (lower left) zero
  rightNonNeg <- D.sgte (lower right) zero
  neitherNeg <- D.and leftNonNeg rightNonNeg
  leftNeg <- D.slt (upper left) zero
  rightNeg <- D.slt (upper right) zero
  bothNeg <- D.and leftNeg rightNeg
  cond1 <- D.or neitherNeg bothNeg

  -- Second conditional: return Negative32
  onlyLeftNeg <- D.and leftNeg rightNonNeg
  onlyRightNeg <- D.and leftNonNeg rightNeg
  cond2 <- D.or onlyLeftNeg onlyRightNeg

  -- Otherwise return Signed 32
  result <- newResultRange "result" D.i32
  error "not sure what we are doing about range types"


-- | https://cs.chromium.org/chromium/src/v8/src/compiler/operation-typer.cc?q=NumberAdd&sq=package:chromium&g=0&l=933
numberShiftLeft :: Range -> Range -> D.Verif Range
numberShiftLeft left right = do
  thirtyOne <- D.i32c 31
  zero <- D.i32c 0
  intmax <- D.i32max
  intmin <- D.i32min

  (upperRight, lowerRight) <- do
    cond <- D.sgt (upper right) thirtyOne
    upper <- D.cond cond thirtyOne (upper right)
    lower <- D.cond cond zero (lower right)
    return (upper, lower)

  min <- do
    minLeft <- D.safeSll (lower left) lowerRight
    minRight <- D.safeSll (lower left) upperRight
    D.smin minLeft minRight

  max <- do
    maxLeft <- D.safeSll (upper left) lowerRight
    maxRight <- D.safeSll (upper left) upperRight
    D.smax maxLeft maxRight

  -- condition one: full range
  --  if (max_lhs > (kMaxInt >> max_rhs) || min_lhs < (kMinInt >> max_rhs))
  condOne <- do
    firstShift <- D.safeSra intmax upperRight
    leftPart <- D.sgt (upper left) firstShift
    secondShift <- D.safeSra intmin upperRight
    rightPart <- D.sgt (lower left) secondShift
    D.or leftPart rightPart

  -- condition two (seems redundant?): full range
  -- if (max == kMaxInt && min == kMinInt)
  largestRange <- do
    isIntMax <- D.eq max intmax
    isIntMin <- D.eq min intmin
    D.and isIntMax isIntMin

  -- condition three: one of those two conditions did not hold, and the result
  -- will just be min and max

  -- Make the result range
  result <- newResultRange "result" D.i32
  tmpLower <- D.cond condOne intmin min
  tmpUpper <- D.cond condOne intmax max
  D.cond largestRange intmin tmpLower >>= D.assign (lower result)
  D.cond largestRange intmax tmpUpper >>= D.assign (upper result)
  return result

-- | https://cs.chromium.org/chromium/src/v8/src/compiler/operation-typer.cc?q=NumberAdd&sq=package:chromium&g=0&l=968
numberShiftRight :: Range -> Range -> D.Verif Range

numberShiftRight left right = do
  thirtyOne <- D.i32c 31
  zero <- D.i32c 0

  rightLarger <- D.sgt (upper right) thirtyOne
  maxRight <- D.cond rightLarger thirtyOne (upper right)
  minRight <- D.cond rightLarger zero (lower right)

  min <- do
    firstShift <- D.safeSra (lower left) minRight
    secondShift <- D.safeSra (lower left) maxRight
    D.smin firstShift secondShift

  max <- do
    firstShift <- D.safeSra (upper left) minRight
    secondShift <- D.safeSra (upper left) maxRight
    D.smin firstShift secondShift

  result <- newResultRange "result" D.i32
  D.assign (lower result) min
  D.assign (upper result) max
  return result

-- | https://cs.chromium.org/chromium/src/v8/src/compiler/operation-typer.cc?q=NumberAdd&sq=package:chromium&g=0&l=993
numberShiftRightLogical :: Range -> Range -> D.Verif Range
numberShiftRightLogical = undefined

