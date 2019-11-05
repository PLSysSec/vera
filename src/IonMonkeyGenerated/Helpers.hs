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
                            , v "lower_u" `assign` (cast (v "u_lower_boud") Signed)
                            , v "upper_u" `assign` (cast (v "u_upper_bound") Signed)
                            , (v "rv") .->. "lower" `assign` (v "lower_u")
                            , (v "rv") .->. "upper" `assign` (v "upper_u")
                            , return_ (v "rv")
                            ]
                 in Function "newUInt32Range" (c "range") args body

int32min :: Codegen SExpr
int32min = n Signed (-2147483648)

int32max :: Codegen SExpr
int32max = n Signed 2147483647

uint32max :: Codegen SExpr
uint32max = n Signed 4294967295

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
