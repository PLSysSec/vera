{-# LANGUAGE QuasiQuotes #-}
module IonMonkeyGenerated.Operations ( add -- FRACT
                                     , sub -- FRACT
                                     , mul -- FRACT
                                     , and -- FRACT
                                     , or -- FRACT
                                     , xor -- FRACT
                                     , not -- FRACT
                                     , lsh -- FRACT
                                     , rsh -- FRACT
                                     , ursh -- needs assumption, FRACT
                                     , lsh' -- FRACT
                                     , rsh' -- FRACT
                                     , ursh' -- needs assumption, FRACT
                                     , abs -- FRACT
                                     , min -- FRACT
                                     , max -- FRACT
                                     , floor -- FRACT. WHY DIDNT THIS VERIFY BEFORE
                                     , ceil -- TEST
                                     , sign -- done
                                     , intersect -- ish
                                     , brokenIntersect
                                     , union
                                     ) where
import           Control.Monad
import           DSL.Typed                  (Type (..))
import           Data.List                  (find)
import           Generate.Lang
import           Generate.QQ
import           IonMonkeyGenerated.Helpers
import           Prelude                    hiding (abs, and, div, floor, max,
                                             min, mod, not, or)

p :: Program
p = [progFile|src/IonMonkeyGenerated/code.cpp|]

prog_func :: Program -> String -> FunctionDef
prog_func (Program fs _) s = case find (\fd -> fName fd == s) fs of
                                      Just func -> func
                                      Nothing -> error "Couldn't find function"

fn :: String -> FunctionDef
fn = prog_func p

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#744
add :: FunctionDef
add = fn "add"

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#775
sub :: FunctionDef
sub = fn "sub"

  -- One try
-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#805
and :: FunctionDef
and = fn "and_"

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#834
or :: FunctionDef
or = fn "or_"

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#893
xor :: FunctionDef
xor = fn "xor_"
  
-- Two trys (neg . not)
-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#955
not :: FunctionDef
not =  fn "not_"

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#960
mul :: FunctionDef
mul = fn "mul"

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#999
lsh :: FunctionDef
lsh = fn "lsh" 

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1016
rsh :: FunctionDef
rsh = fn "rsh"

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1023
ursh :: FunctionDef
ursh = fn "ursh" 

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1042
lsh' :: FunctionDef
lsh' =  fn "lsh_p"

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1048
rsh' :: FunctionDef
rsh' = fn "rsh_p"

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1079
ursh' :: FunctionDef
ursh' = fn "ursh_p"

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1089
abs :: FunctionDef
abs = fn "abs" 

-- Nan thing
-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1104
min :: FunctionDef
min = fn "min"

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1123
max :: FunctionDef
max = fn "max"

-- Add fractional part
-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1142
floor :: FunctionDef
floor = fn "floor" 

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1166
ceil :: FunctionDef
ceil =  fn "ceil"
  
-- Not doing nan thing
-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1184
sign :: FunctionDef
sign = fn "sign"

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#485
intersect :: FunctionDef
intersect = fn "intersect"

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#485
brokenIntersect :: FunctionDef
brokenIntersect =
  let args = [ ("lhs", c "range")
             , ("rhs", c "range")
             ]
      body = [ declare (t Signed) "newLower"
             , declare (t Signed) "newUpper"
             , declare (t Bool) "emptyRange"
             , v "emptyRange" `assign` n Bool 0
             , v "newLower" `assign` max_ (v "lhs" .->. "lower") (v "rhs" .->. "lower")
             , v "newUpper" `assign` min_ (v "lhs" .->. "upper") (v "rhs" .->. "upper")
             , if_ (v "newUpper" .<. v "newLower")
               [ v "emptyRange" `assign` n Bool 1
               , return_ $ call "nullRange" [v "emptyRange"]
               ]  []
             , declare (t Bool) "newHasInt32LowerBound"
             , declare (t Bool) "newHasInt32UpperBound"
             , declare (t Bool) "newMayIncludeNegativeZero"
             , declare (t Bool) "newCanHaveFractionalPart"
             , declare (t Unsigned16) "newExponent"

             , v "newHasInt32LowerBound" `assign` ((v "lhs" .->. "hasInt32LowerBound") .||. (v "rhs" .->. "hasInt32LowerBound"))
             , v "newHasInt32UpperBound" `assign` ((v "lhs" .->. "hasInt32UpperBound") .||. (v "rhs" .->. "hasInt32UpperBound"))

             , v "newCanHaveFractionalPart" `assign` ((v "lhs" .->. "canHaveFractionalPart") .&&. (v "rhs" .->. "canHaveFractionalPart"))
             , v "newMayIncludeNegativeZero" `assign` ( (v "lhs" .->. "canBeNegativeZero") .&&. (v "lhs" .->. "canBeNegativeZero") )

             , v "newExponent" `assign` min_ (v "lhs" .->. "maxExponent") (v "rhs" .->. "maxExponent")

             , if_ (v "newHasInt32LowerBound" .&&. v "newHasInt32UpperBound" .&&. (v "newExponent" .==. includesInfinityAndNan)) [return_ $ call "nullRange" [v "emptyRange"]] []

             , declare (c "range") "intersect_result"
             , v "intersect_result" .->. "lower" `assign` v "newLower"
             , v "intersect_result" .->. "hasInt32LowerBound" `assign` v "newHasInt32LowerBound"
             , v "intersect_result" .->. "upper" `assign` v "newUpper"
             , v "intersect_result" .->. "hasInt32UpperBound" `assign` v "newHasInt32UpperBound"
             , v "intersect_result" .->. "canBeNegativeZero" `assign` v "newMayIncludeNegativeZero"
             , v "intersect_result" .->. "canHaveFractionalPart" `assign` v "newCanHaveFractionalPart"
             , v "intersect_result" .->. "maxExponent" `assign` v "newExponent"
             , v "intersect_result" .->. "isEmpty" `assign` v "emptyRange"
             , return_ $ v "intersect_result"
             ]
   in Function "intersect" (c "range") args body


-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#579
union :: FunctionDef
union = fn "union_"
