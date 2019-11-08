module IonMonkeyGenerated.Helpers where
import           DSL.Typed       (Type (..))
import           Generate.Lang
import           Generate.SMTAST
import           Generate.State

range3 :: FunctionDef
range3 =
  let args = [ ("lower_bound", t Signed64)
             , ("upper_bound", t Signed64)
             , ("nz_flag", t Bool)
             ]
      body = [ declare (c "range") "rv"
             , v "rv" `assign` call "setLowerInit" [ v "lower_bound"
                                                   , v "rv"
                                                   ]
             , v "rv" `assign` call "setUpperInit" [ v "upper_bound"
                                                   , v "rv"
                                                   ]
             , v "rv" .->. "canBeNegativeZero" `assign` (v "nz_flag")
             , return_ (v "rv")
             ]
  in Function "Range3" (c "range") args body

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#394
newInt32Range :: FunctionDef
newInt32Range = let args = [ ("lower_bound", t Signed)
                           , ("upper_bound", t Signed)
                           ]
                    body = [ declare (c "range") "rv"
                           , (v "rv") .->. "lower" `assign` (v "lower_bound")
                           , (v "rv") .->. "upper" `assign` (v "upper_bound")
                           , return_ (v "rv")
                           ]
                in Function "newInt32Range" (c "range") args body

newUInt32Range :: FunctionDef
newUInt32Range = let args = [ ("u_lower_bound", t Unsigned)
                            , ("u_upper_bound", t Unsigned)
                            ]
                     body = [ declare (c "range") "rv"
                            , declare (t Signed) "lower_u"
                            , declare (t Signed) "upper_u"
                            , v "lower_u" `assign` (cast (v "u_lower_bound") Signed)
                            , v "upper_u" `assign` (cast (v "u_upper_bound") Signed)
                            , (v "rv") .->. "lower" `assign` (v "lower_u")
                            , (v "rv") .->. "upper" `assign` (v "upper_u")
                            , return_ (v "rv")
                            ]
                 in Function "newUInt32Range" (c "range") args body

setLowerInit :: FunctionDef
setLowerInit =
  let args = [ ("sli_x", t Signed64)
             , ("sli_range", c "range")
             ]
      body = [ if_ (v "sli_x" .>. jsIntMax64)
               [ v "sli_range" .->. "lower" `assign` jsIntMax
               , v "sli_range" .->. "hasInt32LowerBound" `assign` n Bool 1
               ]
               [ if_ (v "sli_x" .<. jsIntMin64)
                 [ v "sli_range" .->. "lower" `assign` jsIntMin
                 , v "sli_range" .->. "hasInt32LowerBound" `assign` n Bool 0
                 ]
                 [ v "sli_range" .->. "lower" `assign` (cast (v "sli_x") Signed)
                 , v "sli_range" .->. "hasInt32LowerBound" `assign` n Bool 1
                 ]
               ]
             , return_ $ v "sli_range"
             ]
  in Function "setLowerInit" (c "range") args body

setUpperInit :: FunctionDef
setUpperInit =
  let args = [ ("sui_x", t Signed64)
             , ("sui_range", c "range")
             ]
      body = [ if_ (v "sui_x" .>. jsIntMax64)
               [ v "sui_range" .->. "upper" `assign` jsIntMax
               , v "sui_range" .->. "hasInt32UpperBound" `assign` n Bool 0
               ]
               [ if_ (v "sui_x" .<. jsIntMin64)
                 [ v "sui_range" .->. "upper" `assign` jsIntMin
                 , v "sui_range" .->. "hasInt32UpperBound" `assign` n Bool 1
                 ]
                 [ v "sui_range" .->. "upper" `assign` (cast (v "sui_x") Signed)
                 , v "sui_range" .->. "hasInt32UpperBound" `assign` n Bool 1
                 ]
               ]
             , return_ $ v "sui_range"
             ]
  in Function "setUpperInit" (c "range") args body

--- Less complicated stuff

range_constructor :: FunctionDef
range_constructor = undefined

int32min :: Codegen SExpr
int32min = n Signed (-2147483648)

int32max :: Codegen SExpr
int32max = n Signed 2147483647

uint32max :: Codegen SExpr
uint32max = n Unsigned 4294967295

uint32min :: Codegen SExpr
uint32min = n Unsigned 0

excludesNegativeZero :: Codegen SExpr
excludesNegativeZero = n Bool 0

maxFiniteExponent :: Codegen SExpr
maxFiniteExponent = n Unsigned16 1023

includesInfinity :: Codegen SExpr
includesInfinity = n Unsigned16 1 .+. maxFiniteExponent

includesInfinityAndNan :: Codegen SExpr
includesInfinityAndNan = n Unsigned16 65535

noInt32LowerBound :: Codegen SExpr
noInt32LowerBound = (cast jsIntMin Signed64) .-. n Signed64 1

noInt32UpperBound :: Codegen SExpr
noInt32UpperBound = (cast jsIntMax Signed64) .-. n Signed64 1

jsIntMax :: Codegen SExpr
jsIntMax = n Signed (0x7fffffff)

jsIntMin :: Codegen SExpr
jsIntMin = n Signed (0x80000000)

jsIntMax64 :: Codegen SExpr
jsIntMax64 = n Signed64 (0x7fffffff)

jsIntMin64 :: Codegen SExpr
jsIntMin64 = n Signed64 (0x80000000)

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#489
hasInt32Bounds :: FunctionDef
hasInt32Bounds =
  let args = [ ("bnds", c "range") ]
      body = [ return_ $ (v "bnds" .->. "hasInt32LowerBound")  .&&. (v "bnds" .->. "hasInt32UpperBound")
             ]
  in Function "hasInt32Bounds" (t Bool) args body

isFiniteNonNegative :: FunctionDef
isFiniteNonNegative =
  let args = [ ("fnn", c "range") ]
      body = [ return_ $ (v "fnn" .->. "lower" .>. n Signed 0) -- finish this
             ]
  in Function "isFiniteNonNegative" (t Bool) args body

isFiniteNegative :: FunctionDef
isFiniteNegative =
  let args = [ ("fn", c "range") ]
      body = [ return_ $ (v "fn" .->. "upper" .<. n Signed 0) -- finish this
             ]
  in Function "isFiniteNegative" (t Bool) args body

canBeInfiniteOrNan :: FunctionDef
canBeInfiniteOrNan =
  let args = [ ("fnan", c "range") ]
      body = [ return_ $ (v "fnan" .->. "maxExponent" .=>. includesInfinity)
             ]
  in Function "canBeInfiniteOrNan" (t Bool) args body

-- | http://aggregate.org/MAGIC/#Population%20Count%20(Ones%20Count)
countOnes :: FunctionDef
countOnes =
  let args = [ ("y", t Unsigned) ]
      body = [ v "y" .-=. ((v "y" .>>. n Signed 1) .&&. n Unsigned 1431655765)
             , v "y" `assign` (((v "y" .>>. n Unsigned 2) .&&. n Unsigned 858993459) .+. (v "y" .&&. n Unsigned 858993459))
             , v "y" `assign` (((v "y" .>>. n Unsigned 4) .+. v "y").&&. n Unsigned 252645135)
             , v "y" .+=. (v "y" .>>. n Unsigned 8)
             , v "y" .+=. (v "y" .>>. n Unsigned 16)
             , return_ $ v "y" .&&. n Unsigned 63
             ]
  in Function "countOnes" (t Unsigned) args body

countLeadingZeroes :: FunctionDef
countLeadingZeroes =
  let args = [ ("x", t Unsigned) ]
      body = [ v "x" .|=. (v "x" .>>. n Unsigned 1)
             , v "x" .|=. (v "x" .>>. n Unsigned 2)
             , v "x" .|=. (v "x" .>>. n Unsigned 4)
             , v "x" .|=. (v "x" .>>. n Unsigned 8)
             , v "x" .|=. (v "x" .>>. n Unsigned 16)
             , declare (t Unsigned) "ones"
             , v "ones" `assign` call "countOnes" [v "x"]
             , return_ $ n Unsigned 32 .-. v "ones"
             ]
  in Function "countLeadingZeroes" (t Unsigned) args body
