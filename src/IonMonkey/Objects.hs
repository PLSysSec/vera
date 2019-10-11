module IonMonkey.Objects ( Range
                         , Range(..)
                         -- * Getters for fields of the range
                         , lower
                         , upper
                         , hasInt32LowerBound
                         , hasInt32UpperBound
                         , canBeNegativeZero
                         , canHaveFractionalPart
                         , maxExponent
                         , rangeName
                         -- * Making ranges and operands
                         , inputRange
                         , resultRange
                         , operandWithRange
                         ) where
import           Control.Monad         (unless, when)
import           Data.List             (intersperse, isInfixOf)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (catMaybes)
import qualified DSL.DSL               as D
import           DSL.Typed             as T
import qualified DSL.Z3Wrapper         as D
import           IonMonkey.Helpers
import           IonMonkey.ObjectTypes

inputRange :: Type -> String -> D.Verif Range
inputRange ty operandName = do

  -- Make and setup the int32 bounds
  let lowerName = operandName ++ "_lower"
      upperName = operandName ++ "_upper"
      -- If it's not a 32-bit input range, this should be irrelevant
      rangeType = if is32Bits ty then ty else T.Signed
  lowerNode <- T.newInputVar rangeType lowerName
  upperNode <- T.newInputVar rangeType upperName
  T.cppLte lowerNode upperNode >>= T.vassert

  -- Make the flags
  let hasLowerBoundName = operandName ++ "_hasLowerBound"
      hasUpperBoundName = operandName ++ "_hasUpperBound"
      infOrNanName      = operandName ++ "_infOrNan"
      negZeroName       = operandName ++ "_negZero"
      fractPartName     = operandName ++ "_hasFract"
      expName           = operandName ++ "_exp"
  hasLowerBound <- T.newInputVar T.Bool hasLowerBoundName
  hasUpperBound <- T.newInputVar T.Bool hasUpperBoundName
  infOrNan      <- T.newInputVar T.Bool infOrNanName
  negZero       <- T.newInputVar T.Bool negZeroName
  fractPart     <- T.newInputVar T.Bool fractPartName
  exp           <- T.newInputVar T.Unsigned16 expName

  -- Set the flags according to the type: if its an int, all are false,
  -- otherwise they could be anything
  when (is32Bits ty) $ do
    t <- T.true
    f <- T.false
    T.vassign hasLowerBound t
    T.vassign hasUpperBound t
    T.vassign infOrNan f
    T.vassign negZero f
    T.vassign fractPart f
    T.unum16 0 >>= T.vassign exp

  return $ Range operandName lowerNode upperNode hasLowerBound hasUpperBound negZero fractPart exp

resultRange :: Type -> String -> D.Verif Range
resultRange ty operandName = do

  -- Make and setup the int32 bounds
  let lowerName = operandName ++ "_lower"
      upperName = operandName ++ "_upper"
      -- If it's not a 32-bit input range, this should be irrelevant
      rangeType = if is32Bits ty then ty else T.Signed
  lowerNode <- T.newResultVar rangeType lowerName
  upperNode <- T.newResultVar rangeType upperName

  -- Make the flags
  let hasLowerBoundName = operandName ++ "_hasLowerBound"
      hasUpperBoundName = operandName ++ "_hasUpperBound"
      infOrNanName      = operandName ++ "_infOrNan"
      negZeroName       = operandName ++ "_negZero"
      fractPartName     = operandName ++ "_hasFract"
      expName           = operandName ++ "_exp"
  hasLowerBound <- T.newResultVar T.Bool hasLowerBoundName
  hasUpperBound <- T.newResultVar T.Bool hasUpperBoundName
  infOrNan      <- T.newResultVar T.Bool infOrNanName
  negZero       <- T.newResultVar T.Bool negZeroName
  fractPart     <- T.newResultVar T.Bool fractPartName
  exp           <- T.newResultVar T.Unsigned16 expName

  return $ Range operandName lowerNode upperNode hasLowerBound hasUpperBound negZero fractPart exp

-- | Make a new operand with name 'name' of sort 'sort' that is in the range
--'range'---ie is greater than the range's lower and less than the range's upper
operandWithRange :: String -> Type -> Range -> D.Verif T.VNode
operandWithRange name ty range = do
  op <- newInputVar ty name
  if isDouble ty

  -- For doubles, its complicated AF because there are a lot of flags
  then do
    -- If the range says includes inf or nan, the result should be inf or nan
    inf <- T.posInf
    nan <- T.nan

    isInf <- includesInfinityAndNan >>= T.cppEq (maxExponent range)
    isNan <- includesInfinity >>= T.cppEq (maxExponent range)

    T.cppCond isInf inf op >>= T.vassign op
    T.cppCond isNan nan op >>= T.vassign op

    -- If the range doesn't say can-be-neg-zero, it can't be neg zero
    cantBeNegZero <- T.cppNot $ canBeNegativeZero range
    isNeg <- T.isNeg op
    isZero <- T.isZero op
    isNegZero <- T.cppAnd isNeg isZero
    T.cppXor cantBeNegZero isNegZero >>= T.vassert

    -- If it can have a fractional part the range should say so


    -- If it can be outside of a standard int range, the flag should indicate so
    fpJsMax <- T.fpnum 2147483647
    fpJsMin <- T.fpnum (-2147483648)
    isTooBig <- T.cppGt op fpJsMax
    isTooSmall <- T.cppLt op fpJsMin
    f <- T.false
    let hasLower = hasInt32LowerBound range
        hasUpper = hasInt32UpperBound range
    hasLower' <- T.cppCond isTooSmall f hasLower
    hasUpper' <- T.cppCond isTooBig f hasUpper
    T.vassign hasLower hasLower'
    T.vassign hasUpper hasUpper'

  -- For int32s, just make sure the operand is within the range
  else do
    T.cppLte op (upper range) >>= T.vassert
    T.cppGte op (lower range) >>= T.vassert
  return op

