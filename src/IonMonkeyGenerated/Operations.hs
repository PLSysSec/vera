module IonMonkeyGenerated.Operations ( add
                                     , sub
                                     , and -- done
                                     , or -- done
                                     , xor -- done
                                     , not -- done
                                     , mul
                                     , lsh -- done
                                     , rsh -- done
                                     , ursh
                                     , lsh' -- done
                                     , rsh' -- done
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

-- One try
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
or =
  let args = [ ("lhs", c "range")
             , ("rhs", c "range")
             ]
      body = [ if_ ((v "lhs" .->. "lower") .==. (v "lhs" .->. "upper"))
               [ if_ ((v "lhs" .->. "lower") .==. (n Signed 0))
                 [return_ $ v "rhs"] []
               , if_ ((v "lhs" .->. "lower") .==. (n Signed (-1)))
                 [return_ $ v "lhs"] []
               ] []
             , if_ ((v "rhs" .->. "lower") .==. (v "rhs" .->. "upper"))
               [ if_ ((v "rhs" .->. "lower") .==. (n Signed 0))
                 [return_ $ v "lhs"] []
               , if_ ((v "rhs" .->. "lower") .==. (n Signed (-1)))
                 [return_ $ v "rhs"] []
               ] []
             , declare (t Signed) "lower"
             , declare (t Signed) "upper"
             , v "lower" `assign` int32min
             , v "upper" `assign` int32max
             , if_ (((v "lhs" .->. "lower") .=>. n Signed 0) .&&. ((v "rhs" .->. "lower") .=>. n Signed 0))
                   [ v "lower" `assign` (max_ (v "lhs" .->. "lower") (v "rhs" .->. "lower"))
                   , v "upper" `assign` (cast (uint32max .>>. (min_ (call "countLeadingZeroes" [v "lhs" .->. "upper"]) (call "countLeadingZeroes" [v "rhs" .->. "upper"]))) Signed)
                   ]
                   [ declare (t Unsigned) "leadingOnes"
                   , v "leadingOnes" `assign` n Unsigned 0
                   , if_ (v "lhs" .->. "upper" .<. n Signed 0)
                     [ v "leadingOnes" `assign` call "countLeadingZeroes" [not_ $ v "lhs" .->. "lower"]
                     , v "lower" `assign` (max_ (v "lower") (not_ $ cast (uint32max .>>. v "leadingOnes") Signed))
                     , v "upper" `assign` n Signed (-1)
                     ] []
                   , if_ (v "rhs" .->. "upper" .<. n Signed 0)
                     [v "leadingOnes" `assign` call "countLeadingZeroes" [not_ $ v "rhs" .->. "lower"]
                     , v "lower" `assign` (max_ (v "lower") (not_ $ cast (uint32max .>>. v "leadingOnes") Signed))
                     , v "upper" `assign` n Signed (-1) ] []
                   ]
             , return_ $ call "newInt32Range" [v "lower", v "upper"]
             ]
  in Function "or" (c "range") args body


-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#893
xor :: FunctionDef
xor =
  let args = [ ("lhs", c "range")
             , ("rhs", c "range")
             ]
      body = [ declare (t Signed) "lhsLower"
             , declare (t Signed) "lhsUpper"
             , declare (t Signed) "rhsLower"
             , declare (t Signed) "rhsUpper"
             , declare (t Bool)   "invertAfter"
             , declare (t Signed) "tmp"
             , declare (t Unsigned) "lhsLeadingZeroes"
             , declare (t Unsigned) "rhsLeadingZeroes"
             , v "lhsLeadingZeroes" `assign` n Unsigned 0
             , v "rhsLeadingZeroes" `assign` n Unsigned 0
             , v "lhsLower" `assign` (v "lhs" .->. "lower")
             , v "lhsUpper" `assign` (v "lhs" .->. "upper")
             , v "rhsLower" `assign` (v "rhs" .->. "lower")
             , v "rhsUpper" `assign` (v "rhs" .->. "upper")
             , v "invertAfter" `assign` n Bool 0
             , v "tmp" `assign` n Signed 0
             , if_ (v "lhsUpper" .<. n Signed 0)
               [ v "lhsLower" `assign` (not_ $ v "lhsLower")
               , v "lhsUpper" `assign` (not_ $ v "lhsUpper")
               , v "tmp" `assign` v "lhsLower"
               , v "lhsLower" `assign` v "lhsUpper"
               , v "lhsUpper" `assign` v "tmp"
               , v "invertAfter" `assign` (not_ $ v "invertAfter")
               ] []
             , if_ (v "rhsUpper" .<. n Signed 0)
               [ v "rhsLower" `assign` (not_ $ v "rhsLower")
               , v "rhsUpper" `assign` (not_ $ v "rhsUpper")
               , v "tmp" `assign` v "rhsLower"
               , v "rhsLower" `assign` v "rhsUpper"
               , v "rhsUpper" `assign` v "tmp"
               , v "invertAfter" `assign` (not_ $ v "invertAfter")
               ] []
             , declare (t Signed) "lower"
             , declare (t Signed) "upper"
             , v "lower" `assign` int32min
             , v "upper" `assign` int32max
             , if_ ((v "lhsLower" .==. n Signed 0) .&&. (v "lhsUpper" .==. n Signed 0))
               [ v "upper" `assign` v "rhsUpper"
               , v "lower" `assign` v "rhsLower"
               ]
               [if_ ((v "rhsLower" .==. n Signed 0) .&&. (v "rhsUpper" .==. n Signed 0))
                [ v "upper" `assign` v "lhsUpper"
                , v "lower" `assign` v "lhsLower"
                ]
                [if_ ((v "lhsLower" .=>. n Signed 0) .&&. (v "rhsLower" .=>. n Signed 0))
                   [ v "lower" `assign` n Signed 0
                   , v "lhsLeadingZeroes" `assign` (call "countLeadingZeroes" [v "lhsUpper"])
                   , v "rhsLeadingZeroes" `assign` (call "countLeadingZeroes" [v "rhsUpper"])
                   , v "upper" `assign` (min_ (v "rhsUpper" .||. (cast (uint32max .>>. v "lhsLeadingZeroes") Signed)) (v "lhsUpper" .||. (cast (uint32max .>>. v "lhsLeadingZeroes") Signed)))
                   ] []
                ]
               ]
             , if_ (v "invertAfter")
               [ v "lower" `assign` (not_ $ v "lower")
               , v "upper" `assign` (not_ $ v "upper")
               , v "tmp" `assign` v "lower"
               , v "lower" `assign` v "upper"
               , v "upper" `assign` v "tmp"
               ] []
             , return_ $ call "newInt32Range" [v "lower", v "upper"]
             ]
  in Function "xor" (c "range") args body

-- Two trys (neg -> not)
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
lsh =
  let args = [ ("lhs", c "range")
             , ("c", t Signed)
             ]
      body = [ declare (t Signed) "shift"
             , v "shift" `assign` (v "c" .&&. n Signed 31)
             , if_ (((cast (((((cast (v "lhs" .->. "lower") Unsigned) .<<. v "shift")  .<<. n Signed 1) .>>. v "shift") .>>. n Signed 1) Signed) .==. ((cast (((((cast (v "lhs" .->. "upper") Unsigned) .<<. v "shift")  .<<. n Signed 1) .>>. v "shift") .>>. n Signed 1) Signed))))
               [return_ $ call "newInt32Range" [ (cast (v "lhs" .->. "lower") Unsigned) .<<. v "shift"
                                               , (cast (v "lhs" .->. "upper") Unsigned) .<<. v "shift"
                                               ]
               ] []
             , return_ $ call "newInt32Range" [int32min, int32max]
             ]
  in Function "lsh" (c "range") args body


-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1016
rsh :: FunctionDef
rsh =
  let args = [ ("lhs", c "range")
             , ("c", t Signed)
             ]
      body = [ declare (t Signed) "shift"
             , v "shift" `assign` (v "c" .&&. n Signed 31)
             , return_ $ call "newInt32Range" [ (v "lhs" .->. "lower") .>>. v "shift"
                                              , (v "lhs" .->. "upper") .>>. v "shift"
                                              ]
             ]
  in Function "rsh" (c "range") args body


-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1023
ursh :: FunctionDef
ursh =
  let args = [ ("lhs", c "range")
             , ("c", t Signed)
             ]
      body = [ declare (t Signed) "shift"
             , v "shift" `assign` (v "c" .&&. n Signed 31)
             , if_ ((call "isFiniteNonNegative" [v "lhs"]) .||. (call "isFiniteNegative" [v "lhs"]))
               [ return_ $ call "newUInt32Range" [ cast ((v "lhs" .->. "lower") .>>. v "shift") Unsigned
                                                 , cast ((v "lhs" .->. "upper") .>>. v "shift") Unsigned
                                                 ]
               ] []
             , return_ $ call "newUInt32Range" [ n Unsigned 0
                                               , uint32max .>>. v "shift"
                                               ]
             ]
  in Function "ursh" (c "range") args body

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1042
lsh' :: FunctionDef
lsh' = let body = [ return_ $ call "newInt32Range" [ int32min, int32max ] ]
           args = [ ("lhs", c "range")
                  , ("rhs", c "range")
                  ]
       in Function "lsh'" (c "range") args body

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1048
rsh' :: FunctionDef
rsh' =
  let args = [ ("lhs", c "range")
             , ("rhs", c "range")
             ]
      body = [ declare (t Signed) "shiftLower"
             , declare (t Signed) "shiftUpper"
             , v "shiftLower" `assign` (v "rhs" .->. "lower")
             , v "shiftUpper" `assign` (v "rhs" .->. "upper")
             , if_ (((cast (v "shiftUpper") Signed64) .-. (cast (v "shiftLower") Signed64)) .<. (n Signed64 31))
               [ v "shiftLower" `assign` (n Signed 0)
               , v "shiftUpper" `assign` (n Signed 31)
               ]
               [ v "shiftLower" `assign` ((v "shiftLower") .&&. (n Signed 31))
               , v "shiftUpper" `assign` ((v "shiftUpper") .&&. (n Signed 31))
               , if_ (v "shiftLower" .<. v "shiftUpper")
                 [ v "shiftLower" `assign` (n Signed 0)
                 , v "shiftUpper" `assign` (n Signed 31)
                 ] []
               ]
               , declare (t Signed) "lhsLower"
               , declare (t Signed) "min"
               , declare (t Signed) "lhsUpper"
               , declare (t Signed) "max"
               , v "lhsLower" `assign` (v "lhs" .->. "lower")
               , v "min" `assign` tern_ (v "lhsLower" .<. (n Signed 0)) (v "lhsLower" .>>. v "shiftLower") (v "lhsLower" .>>. v "shiftUpper")
               , v "lhsUpper" `assign` (v "lhs" .->. "upper")
               , v "max" `assign` tern_ (v "lhsUpper" .=>. (n Signed 0)) (v "lhsUpper" .>>. v "shiftLower") (v "lhsUpper" .>>. v "shiftUpper")
               , return_ $ call "newInt32Range" [v "min", v "max"]
             ]
  in Function "rsh'" (c "range") args body


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


