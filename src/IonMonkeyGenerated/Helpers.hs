module IonMonkeyGenerated.Helpers where
import           DSL.Typed       (Type (..))
import           Generate.Lang
import           Generate.SMTAST
import           Generate.State

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

range_constructor :: FunctionDef
range_constructor = undefined

int32min :: Codegen SExpr
int32min = n Signed (-2147483648)

int32max :: Codegen SExpr
int32max = n Signed 2147483647

uint32max :: Codegen SExpr
uint32max = n Unsigned 4294967295

excludesNegativeZero :: Codegen SExpr
excludesNegativeZero = n Bool 0

maxFiniteExponent :: Codegen SExpr
maxFiniteExponent = n Unsigned16 1023

includesInfinity :: Codegen SExpr
includesInfinity = n Unsigned16 1 .+. maxFiniteExponent

includesInfinityAndNan :: Codegen SExpr
includesInfinityAndNan = n Unsigned16 65535

noInt32LowerBound :: Codegen SExpr
noInt32LowerBound = undefined

noInt32UpperBound :: Codegen SExpr
noInt32UpperBound = undefined

jsIntMax :: Codegen SExpr
jsIntMax = undefined

jsIntMin :: Codegen SExpr
jsIntMin = undefined

hasInt32Bounds :: FunctionDef
hasInt32Bounds = undefined

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
             , v "x" .|=. (v "x" .>>. n Unsigned 8)
             , declare (t Unsigned) "ones"
             , v "ones" `assign` call "countOnes" [v "x"]
             , return_ $ n Unsigned 32 .-. v "ones"
             ]
  in Function "countLeadingZeroes" (t Unsigned) args body
