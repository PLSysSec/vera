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

int32min :: Codegen SExpr
int32min = n Signed (-2147483648)

int32max :: Codegen SExpr
int32max = n Signed 2147483647

countOnes :: FunctionDef
countOnes =
  let args = [ ("y", t Signed) ]
      body = [ v "y" .-=. ((v "y" .>>. n Signed 1) .&&. n Signed 1431655765)
             , v "y" `assign` (((v "y" .>>. n Signed 2) .&&. n Signed 858993459) .+. (v "y" .&&. n Signed 858993459))
             , v "y" `assign` ((v "y" .>>. n Signed 4) .&&. n Signed 252645135)
             , v "y" .+=. (v "y" .>>. n Signed 8)
             , v "y" .+=. (v "y" .>>. n Signed 16)
             , return_ $ v "y" .&&. n Signed 64
             ]
  in Function "countOnes" (t Signed) args body

countLeadingZeroes :: FunctionDef
countLeadingZeroes =
  let args = [ ("x", t Signed) ]
      body = [ v "x" .|=. (v "x" .>>. n Signed 1)
             , v "x" .|=. (v "x" .>>. n Signed 2)
             , v "x" .|=. (v "x" .>>. n Signed 4)
             , v "x" .|=. (v "x" .>>. n Signed 8)
             , v "x" .|=. (v "x" .>>. n Signed 8)
             , declare (t Signed) "ones"
             , v "ones" `assign` call "countOnes" [v "x"]
             , return_ $ n Signed 32 .-. v "ones"
             ]
  in Function "countLeadingZeroes" (t Signed) args body
