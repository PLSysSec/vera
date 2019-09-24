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
numberBitwiseAnd = undefined

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

