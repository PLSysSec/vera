module IonMonkeyGenerated.Operations ( add
                                     , sub
                                     , and -- done
                                     , or
                                     , xor
                                     , not -- done
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
or = undefined
  -- let args = [ ("lhs", c "range")
  --            , ("rhs", c "range")
  --            ]
  --     body = [ if_ ((v "lhs" .->. "lower") .==. (v "lhs" .->. "upper"))
  --              [ if_ ((v "lhs" .->. "lower") .==. (n Signed 0))
  --                [return_ $ v "rhs"] []
  --              , if_ ((v "lhs" .->. "lower") == (n Signed -1))
  --                [return_ $ v "lhs"] []
  --              ] []
  --            , if_ ((v "rhs" .->. "lower") .==. (v "rhs" .->. "upper"))
  --              [ if_ ((v "rhs" .->. "lower") .==. (n Signed 0))
  --                [return_ $ v "lhs"] []
  --              , if_ ((v "rhs" .->. "lower") .==. (n Signed -1))
  --                [return_ $ v "rhs"] []
  --              ]

  --            ]


-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#893
xor :: FunctionDef
xor = undefined

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
lsh = undefined
  -- let args = [ ("lhs", c "range")
  --            , ("c", t Signed)
  --            ]
  --     body = [ declare (t Signed) "shift"
  --            , if ( (cast ((cast Unsigned (v "lhs" .->. "lower") .<<. v "shift" .<<. (n Unsigned 1) .>>. v "shift" .>>. (n Unsigned 1)) Signed) .&&.
  --                  (cast (cast Unsigned (v) Signed)
  --                 ) [] []
  --            ]

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
             ]
  in error ""

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1042
lsh' :: FunctionDef
lsh' = undefined

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


