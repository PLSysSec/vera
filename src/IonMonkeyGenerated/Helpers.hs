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
