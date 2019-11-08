module IonMonkeyGenerated.Operations ( add -- done
                                     , sub -- done
                                     , mul -- done
                                     , and -- done VER
                                     , or -- done
                                     , xor -- done
                                     , not -- done VER
                                     , lsh -- done
                                     , rsh -- done
                                     , ursh -- done
                                     , lsh' -- done
                                     , rsh' -- done
                                     , ursh' -- done
                                     , abs -- done
                                     , min -- done
                                     , max -- done
                                     , floor -- done
                                     , ceil -- done
                                     , sign -- done
                                     , intersect -- ish
                                     , union
                                     ) where
import           Control.Monad
import           DSL.Typed                  (Type (..))
import           Generate.Lang
import           IonMonkeyGenerated.Helpers
import           Prelude                    hiding (abs, and, floor, max, min,
                                             not, or)

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#744
add :: FunctionDef
add =
  let args = [ ("lhs", c "range")
             , ("rhs", c "range")
             ]
      body = [ declare (t Signed64) "l"
             , declare (t Signed64) "h"
             , declare (t Unsigned16) "e"
             , v "l" `assign` ((cast (v "lhs" .->. "lower") Signed64) .+. (cast (v "rhs" .->. "lower") Signed64))
             , if_ ((not_ $ v "lhs" .->. "hasInt32LowerBound") .||. (not_ $ v "rhs" .->. "hasInt32LowerBound")) [v "l" `assign` noInt32LowerBound] []
             , v "h" `assign` ((cast (v "lhs" .->. "upper") Signed64) .+. (cast (v "rhs" .->. "upper") Signed64))
             , if_ ((not_ $ v "lhs" .->. "hasInt32UpperBound") .||. (not_ $ v "rhs" .->. "hasInt32UpperBound")) [v "h" `assign` noInt32UpperBound] []
             , v "e" `assign` (max_ (v "lhs" .->. "maxExponent") (v "rhs" .->. "maxExponent"))
             , if_ (v "e" .<. maxFiniteExponent) [v "e" .+=. n Unsigned16 1] []
             , if_ ((call "canBeInfiniteOrNan" [v "lhs"]) .&&. (call "canBeInfiniteOrNan" [v "rhs"])) [v "e" `assign` includesInfinityAndNan] []
             , return_ $ call "Range3" [ v "l"
                                       , v "h"
                                       , (v "lhs" .->. "canBeNegativeZero") .&&. (v "rhs" .->. "canBeNegativeZero")
                                       ]
             ]
  in Function "add" (c "range") args body

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#775
sub :: FunctionDef
sub =
  let args = [ ("lhs", c "range")
             , ("rhs", c "range")
             ]
      body = [ declare (t Signed64) "l"
             , declare (t Signed64) "h"
             , declare (t Unsigned16) "e"
             , v "l" `assign` ((cast (v "lhs" .->. "lower") Signed64) .-. (cast (v "rhs" .->. "lower") Signed64))
             , if_ ((not_ $ v "lhs" .->. "hasInt32LowerBound") .||. (not_ $ v "rhs" .->. "hasInt32LowerBound")) [v "l" `assign` noInt32LowerBound] []
             , v "h" `assign` ((cast (v "lhs" .->. "upper") Signed64) .-. (cast (v "rhs" .->. "upper") Signed64))
             , if_ ((not_ $ v "lhs" .->. "hasInt32UpperBound") .||. (not_ $ v "rhs" .->. "hasInt32UpperBound")) [v "h" `assign` noInt32UpperBound] []
             , v "e" `assign` (max_ (v "lhs" .->. "maxExponent") (v "rhs" .->. "maxExponent"))
             , if_ (v "e" .<. maxFiniteExponent) [v "e" .+=. n Unsigned16 1] []
             , if_ ((call "canBeInfiniteOrNan" [v "lhs"]) .&&. (call "canBeInfiniteOrNan" [v "rhs"])) [v "e" `assign` includesInfinityAndNan] []
             , return_ $ call "Range3" [ v "l"
                                       , v "h"
                                       , (v "lhs" .->. "canBeNegativeZero") .&&. (v "rhs" .->. "canBeNegativeZero")
                                       ]
             ]
  in Function "sub" (c "range") args body

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
             , declare (t Unsigned) "clzLhs"
             , declare (t Unsigned) "clzRhs"
             , v "lower" `assign` int32min
             , v "upper" `assign` int32max
             , v "clzLhs" `assign` uint32min
             , v "clzRhs" `assign` uint32min
             ,  declare (t Unsigned) "leadingOnes"
             , v "leadingOnes" `assign` n Unsigned 0
             , if_ (((v "lhs" .->. "lower") .=>. n Signed 0) .&&. ((v "rhs" .->. "lower") .=>. n Signed 0))
                   [ v "lower" `assign` (max_ (v "lhs" .->. "lower") (v "rhs" .->. "lower"))
                   , v "clzLhs" `assign` (call "countLeadingZeroes" [cast (v "lhs" .->. "upper") Unsigned])
                   , v "clzRhs" `assign` (call "countLeadingZeroes" [cast (v "rhs" .->. "upper") Unsigned])
                   , v "upper" `assign` (cast (uint32max .>>. (min_ (v "clzLhs") (v "clzRhs"))) Signed)
                   ]
                   [ if_ (v "lhs" .->. "upper" .<. n Signed 0)
                     [ v "leadingOnes" `assign` call "countLeadingZeroes" [cast (not_ $ v "lhs" .->. "lower") Unsigned]
                     , v "lower" `assign` (max_ (v "lower") (not_ $ cast (uint32max .>>. v "leadingOnes") Signed))
                     , v "upper" `assign` n Signed (-1)
                     ] []
                   , if_ (v "rhs" .->. "upper" .<. n Signed 0)
                     [v "leadingOnes" `assign` call "countLeadingZeroes" [cast (not_ $ v "rhs" .->. "lower") Unsigned]
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
             , declare (t Signed) "upOr"
             , declare (t Signed) "downOr"
             , v "lower" `assign` int32min
             , v "upper" `assign` int32max
             , v "upOr" `assign` int32min
             , v "downOr" `assign` int32max
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
                   , v "lhsLeadingZeroes" `assign` (call "countLeadingZeroes" [cast (v "lhsUpper") Unsigned])
                   , v "rhsLeadingZeroes" `assign` (call "countLeadingZeroes" [cast (v "rhsUpper") Unsigned])
                   , v "upOr" `assign` (v "rhsUpper" .||. (cast (uint32max .>>. v "lhsLeadingZeroes") Signed))
                   , v "downOr" `assign` (v "lhsUpper" .||. (cast (uint32max .>>. v "rhsLeadingZeroes") Signed))
                   , v "upper" `assign` min_ (v "upOr") (v "downOr")
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
mul =
  let args = [ ("lhs", c "range")
             , ("rhs", c "range")
             ]
      body = [ declare (t Bool) "mayIncludeNegativeZero"
             , v "mayIncludeNegativeZero" `assign` (((call "canHaveSignBitSet" [v "lhs"]) .&&. (call "canBeFiniteNonNegative" [v "rhs"])) .||. ((call "canHaveSignBitSet" [v "rhs"]) .&&. (call "canBeFiniteNonNegative" [v "lhs"])))
             , declare (t Unsigned16) "exponent"
             , if_ ((not_ $ call "canBeInfiniteOrNan" [v "lhs"]) .&&. (not_ $ call "canBeInfiniteOrNan" [v "rhs"]))
               [v "exponent" `assign` ((call "numBits" [v "lhs"]) .+. (call "numBits" [v "rhs"]) .-. n Unsigned16 1)
               , if_ (v "exponent" .>. maxFiniteExponent)
                 [v "exponent" `assign` includesInfinity] []
               ]
               [if_ ((not_ $ call "canBeNan" [v "lhs"]) .&&. (not_ $ call "canBeNan" [v "rhs"]) .&&. (not_ $ (call "canBeZero" [v "lhs"]) .&&. (call "canBeInfiniteOrNan" [v "rhs"])) .&&. (not_ $ (call "canBeZero" [v "rhs"]) .&&. (call "canBeInfiniteOrNan" [v "lhs"]) ))
                  [v "exponent" `assign` includesInfinity]
                  [v "exponent" `assign` includesInfinityAndNan]
               ]
             , if_ (call "missingAnyInt32Bounds" [v "lhs", v "rhs"])
               [return_ $ call "Range" [ noInt32LowerBound
                                       , noInt32UpperBound
                                       , v "newMayIncludeNegativeZero"
                                       , v "exponent"
                                       ]
               ] []
             , declare (t Signed64) "a"
             , declare (t Signed64) "b"
             , declare (t Signed64) "c"
             , declare (t Signed64) "d"
             , v "a" `assign` ((cast (v "lhs" .->. "lower") Signed64) .*. (cast (v "rhs" .->. "lower") Signed64))
             , v "b" `assign` ((cast (v "lhs" .->. "lower") Signed64) .*. (cast (v "rhs" .->. "upper") Signed64))
             , v "c" `assign` ((cast (v "lhs" .->. "upper") Signed64) .*. (cast (v "rhs" .->. "lower") Signed64))
             , v "d" `assign` ((cast (v "lhs" .->. "upper") Signed64) .*. (cast (v "rhs" .->. "upper") Signed64))
             , return_ $ call "Range" [ min_ (min_ (v "a") (v "b")) (min_ (v "c") (v "d"))
                                      , max_ (max_ (v "a") (v "b")) (max_ (v "c") (v "d"))
                                      , v "newMayIncludeNegativeZero"
                                      , v "exponent"
                                      ]
             ]
  in Function "mul" (c "range") args body

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#999
lsh :: FunctionDef
lsh =
  let args = [ ("lhs", c "range")
             , ("c", t Signed)
             ]
      body = [ declare (t Signed) "shift"
             , v "shift" `assign` (v "c" .&&. n Signed 31)
             , if_ (((cast (((((cast (v "lhs" .->. "lower") Unsigned) .<<. v "shift")  .<<. n Signed 1) .>>. v "shift") .>>. n Signed 1) Signed) .==. ((cast (((((cast (v "lhs" .->. "upper") Unsigned) .<<. v "shift")  .<<. n Signed 1) .>>. v "shift") .>>. n Signed 1) Signed))))
               [return_ $ call "newInt32Range" [ cast ((cast (v "lhs" .->. "lower") Unsigned) .<<. v "shift") Signed
                                               , cast ((cast (v "lhs" .->. "upper") Unsigned) .<<. v "shift") Signed
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
             , if_ (((cast (v "shiftUpper") Signed64) .-. (cast (v "shiftLower") Signed64)) .=>. (n Signed64 31))
               [ v "shiftLower" `assign` (n Signed 0)
               , v "shiftUpper" `assign` (n Signed 31)
               ]
               [ v "shiftLower" `assign` ((v "shiftLower") .&&. (n Signed 31))
               , v "shiftUpper" `assign` ((v "shiftUpper") .&&. (n Signed 31))
               , if_ (v "shiftLower" .>. v "shiftUpper")
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
ursh' =
  let args = [ ("lhs", c "range")
             , ("rhs", c "range")
             ]
      body = [return_ $ call "newUInt32Range" [ n Unsigned 0
                                              , tern_ (call "isFiniteNonNegative" [v "lhs"]) (cast (v "lhs" .->. "upper") Unsigned) uint32max
                                              ]
             ]
  in Function "ursh'" (c "range") args body

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1089
abs :: FunctionDef
abs =
  let args = [("op", c "range")]
      body = [ declare (t Signed) "l"
             , declare (t Signed) "u"
             , v "l" `assign` (v "op" .->. "lower")
             , v "u" `assign` (v "op" .->. "upper")
             , return_ $ call "Range" [ max_ (max_ (n Signed 0) (v "l")) (tern_ (v "u" .==. int32min) int32max (neg_ $ v "u"))
                                      , n Bool 1
                                      , max_ (max_ (n Signed 0) (v "u")) (tern_ (v "l" .==. int32min) int32max (neg_ $ v "l"))
                                      , (call "hasInt32Bounds" [v "op"]) .&&. (v "l" .!=. int32min)
                                      , excludesNegativeZero
                                      , v "op" .->. "maxExponent"
                                      ]
             ]
  in Function "abs" (c "range") args body

-- Nan thing
-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1104
min :: FunctionDef
min =
  let args = [ ("lhs", c "range")
             , ("rhs", c "range")
             ]
      body = [ return_ $ call "Range" [ min_ (v "lhs" .->. "lower") (v "rhs" .->. "lower")
                                      , (v "lhs" .->. "hasInt32LowerBound") .&&. (v "rhs" .->. "hasInt32UpperBound")
                                      , min_ (v "lhs" .->. "upper") (v "rhs" .->. "upper")
                                      , (v "lhs" .->. "hasInt32UpperBound") .||. (v "rhs" .->. "hasInt32UpperBound")
                                      , (v "lhs" .->. "canBeNegativeZero") .||. (v "rhs" .->. "canBeNegativeZero")
                                      , max_ (v "lhs" .->. "maxExponent") (v "rhs" .->. "maxExponent")
                                      ]
             ]
  in Function "min" (c "range") args body

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1123
max :: FunctionDef
max =
  let args = [ ("lhs", c "range")
             , ("rhs", c "range")
             ]
      body = [ return_ $ call "Range" [ max_ (v "lhs" .->. "lower") (v "rhs" .->. "lower")
                                      , (v "lhs" .->. "hasInt32LowerBound") .||. (v "rhs" .->. "hasInt32UpperBound")
                                      , max_ (v "lhs" .->. "upper") (v "rhs" .->. "upper")
                                      , (v "lhs" .->. "hasInt32UpperBound") .&&. (v "rhs" .->. "hasInt32UpperBound")
                                      , (v "lhs" .->. "canBeNegativeZero") .||. (v "rhs" .->. "canBeNegativeZero")
                                      , max_ (v "lhs" .->. "maxExponent") (v "rhs" .->. "maxExponent")
                                      ]
             ]
  in Function "max" (c "range") args body

-- Add fractional part
-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1142
floor :: FunctionDef
floor =
  let args = [ ("op", c "range") ]
      body = [ declare (c "range") "copy"
             , v "range" `assign` v "op"
               -- missing fract check
             , if_ (v "op" .->. "hasInt32LowerBound")
               [v "copy" `assign` (call "setLowerInit" [(cast (v "copy" .->. "lower") Signed64) .-. n Signed64 1]) ] []
             , if_ (call "hasInt32Bounds" [v "copy"])
               [(v "copy" .->. "maxExponent") `assign` (call "exponentImpliedByInt32Bounds" [v "copy"])]
               [if_ (v "copy" .->. "maxExponent" .<. maxFiniteExponent)
                 [v "copy" .->. "maxExponent" .+=. n Unsigned16 1] []
               ]
             , return_ $ v "copy"
             ]
  in Function "floor" (c "range") args body

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1166
ceil :: FunctionDef
ceil =
  let args = [ ("op", c "range") ]
      body = [ declare (c "range") "copy"
             , v "range" `assign` v "op"
               -- missing fract check
             , if_ (call "hasInt32Bounds" [v "copy"])
               [(v "copy" .->. "maxExponent") `assign` (call "exponentImpliedByInt32Bounds" [v "copy"])]
               [if_ (v "copy" .->. "maxExponent" .<. maxFiniteExponent)
                 [v "copy" .->. "maxExponent" .+=. n Unsigned16 1] []
               ]
             , return_ $ v "copy"
             ]
  in Function "floor" (c "range") args body

-- Not doing nan thing
-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1184
sign :: FunctionDef
sign =
  let args = [ ("op", c "range") ]
      body = [ return_ $ call "Range" [ max_ ( min_ (v "op" .->. "lower") (n Signed 1) ) (n Signed (-1))
                                      , max_ ( min_ (v "op" .->. "upper") (n Signed 1) ) (n Signed (-1))
                                      , v "op" .->. "canBeNegativeZero"
                                      , n Unsigned16 0
                                      ]
             ]
  in Function "sign" (c "range") args body

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#485
intersect :: FunctionDef
intersect =
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
               [if_ ((call "canBeNan" [v "lhs"]) .||. (call "canBeNan" [v "rhs"]))
                  [v "emptyRange" `assign` n Bool 1]
                  []
               ] [] -- Return nptr
             , declare (t Bool) "newHasInt32LowerBound"
             , declare (t Bool) "newHasInt32UpperBound"
             , declare (t Bool) "newMayIncludeNegativeZero"
             , declare (t Unsigned16) "newExponent"
             , v "newHasInt32LowerBound" `assign` ((v "lhs" .->. "hasInt32LowerBound") .||. (v "rhs" .->. "hasInt32LowerBound"))
             , v "newHasInt32UpperBound" `assign` ((v "lhs" .->. "hasInt32UpperBound") .||. (v "rhs" .->. "hasInt32UpperBound"))
             , v "newMayIncludeNegativeZero" `assign` ( (v "lhs" .->. "canBeNegativeZero") .&&. (v "lhs" .->. "canBeNegativeZero") )
             , v "newExponent" `assign` min_ (v "lhs" .->. "maxExponent") (v "rhs" .->. "maxExponent")
             , return_ $ call "Range" [ v "newLower"
                                      , v "newHasInt32LowerBound"
                                      , v "newUpper"
                                      , v "newHasInt32UpperBound"
                                      , v "newMayIncludeNegativeZero"
                                      , v "newExponent"
                                      ]
             ]
  in Function "intersect" (c "range") args body

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#579
union :: FunctionDef
union = undefined
