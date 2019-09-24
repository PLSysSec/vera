module V8.Operations where
import qualified DSL.DSL           as D
import           IonMonkey.Objects

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
numberBitwiseXor = undefined

-- | https://cs.chromium.org/chromium/src/v8/src/compiler/operation-typer.cc?q=NumberAdd&sq=package:chromium&g=0&l=933
numberShiftLeft :: Range -> Range -> D.Verif Range
numberShiftLeft = undefined

-- | https://cs.chromium.org/chromium/src/v8/src/compiler/operation-typer.cc?q=NumberAdd&sq=package:chromium&g=0&l=968
numberShiftRight :: Range -> Range -> D.Verif Range
numberShiftRight = undefined

-- | https://cs.chromium.org/chromium/src/v8/src/compiler/operation-typer.cc?q=NumberAdd&sq=package:chromium&g=0&l=993
numberShiftRightLogical :: Range -> Range -> D.Verif Range
numberShiftRightLogical = undefined

