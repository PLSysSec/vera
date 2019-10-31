module IonMonkeyGenerated.Operations ( add
                                     , sub
                                     , and
                                     , or
                                     , xor
                                     , not
                                     , mul
                                     , lsh
                                     , rsh
                                     , ursh
                                     , lsh'
                                     , rsh'
                                     , ursh'
                                     , abs
                                     , min
                                     , max
                                     , floor
                                     , ceil
                                     , sign
                                     ) where
import           Control.Monad
import           DSL.Typed                  (Type (..))
import           Generate.Lang
import           IonMonkeyGenerated.Helpers
import           Prelude                    hiding (abs, and, floor, max, min,
                                             not, or)

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#744
add :: FunctionDef
add = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#775
sub :: FunctionDef
sub = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#805
and :: FunctionDef
and =
  let args = [ ("lhs", c "range")
             , ("rhs", c "range")
             ]
      body = [ if_
               (((v "lhs" .->. "lower") .<. (n Signed 0)) .&&. ((v "rhs" .->. "lower") .<. (n Signed 0)))
               [return_ $ call "newInt32Range" [ int32min
                                              , max_ (v "lhs" .->. "upper") (v "rhs" .->. "upper")
                                              ] ] []
             , declare (t Signed) "lower_"
             , declare (t Signed) "upper_"
             , (v "lower_") `assign` (n Signed 0)
             , (v "upper_") `assign` min_ (v "lhs" .->. "upper") (v "rhs" .->. "upper")
             , if_ ((v "lhs" .->. "lower") .<. (n Signed 0)) [v "upper_" `assign` (v "rhs" .->. "upper")] []
             , if_ ((v "rhs" .->. "lower") .<. (n Signed 0)) [v "upper_" `assign` (v "lhs" .->. "upper")] []
             , return_ $ call "newInt32Range" [v "lower_", v "upper_"]
             ]
      in Function "and" (c "range") args body

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#834
or :: FunctionDef
or = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#893
xor :: FunctionDef
xor = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#955
not :: FunctionDef
not = let args = [ ("op", c "range") ]
          body = [ declare (c "range") "result_range"
                 , (v "result_range") .->. "lower" `assign` (not_ $ (v "op") .->. "upper")
                 , (v "result_range") .->. "upper" `assign` (not_ $ (v "op") .->. "lower")
                 , return_ $ v "result_range"
                 ]
      in Function "not" (c "range") args body

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#960
mul :: FunctionDef
mul = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#999
lsh :: FunctionDef
lsh = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1016
rsh :: FunctionDef
rsh = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1023
ursh :: FunctionDef
ursh = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1042
lsh' :: FunctionDef
lsh' = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1048
rsh' :: FunctionDef
rsh' = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1079
ursh' :: FunctionDef
ursh' = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1089
abs :: FunctionDef
abs = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1104
min :: FunctionDef
min = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1123
max :: FunctionDef
max = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1142
floor :: FunctionDef
floor = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1166
ceil :: FunctionDef
ceil = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1184
sign :: FunctionDef
sign = undefined


