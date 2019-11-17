{-# LANGUAGE QuasiQuotes #-}
module IonMonkeyGenerated.Helpers where
import           Data.String.Interpolate
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
      body = [ declare (c "range") "rv" -- You have to do this shit now.
             , declare (c "range") "tmp" -- Can't assign and pass thing into class
             , assert_ $ not_ $ undef $ v "tmp" .->. "lower"
             , assert_ $ not_ $ undef $ v "tmp" .->. "upper"
             , assert_ $ not_ $ undef $ v "rv" .->. "lower"
             , assert_ $ not_ $ undef $ v "rv" .->. "upper"
             , v "tmp" `assign` v "rv"
             , v "rv" `assign` call "setLowerInit" [ v "lower_bound"
                                                   , v "tmp"
                                                   ]
             , declare (c "range") "tmp2"
             , v "tmp2" `assign` v "rv"
             , v "rv" `assign` call "setUpperInit" [ v "upper_bound"
                                                   , v "tmp2"
                                                   ]
             , v "rv" .->. "canBeNegativeZero" `assign` (v "nz_flag")
             , return_ $ call "optimize" [v "rv"]
             -- , return_ $ v "rv"
             ]
  in Function "Range3" (c "range") args body

range4 :: FunctionDef
range4 =
  let args = [ ("lower_bound", t Signed64)
             , ("upper_bound", t Signed64)
             , ("fract_flag", t Bool)
             , ("nz_flag", t Bool)
             , ("exp_set", t Unsigned16)
             ]
      body = [ declare (c "range") "rv"
             , declare (c "range") "tmp"
             , assert_ $ not_ $ undef $ v "tmp" .->. "lower"
             , assert_ $ not_ $ undef $ v "tmp" .->. "upper"
             , assert_ $ not_ $ undef $ v "rv" .->. "lower"
             , assert_ $ not_ $ undef $ v "rv" .->. "upper"
              , v "tmp" `assign` v "rv"
             , v "rv" `assign` call "setLowerInit" [ v "lower_bound"
                                                   , v "tmp"
                                                   ]
             , declare (c "range") "tmp2"
             , v "tmp2" `assign` v "rv"
             , v "rv" `assign` call "setUpperInit" [ v "upper_bound"
                                                   , v "tmp2"
                                                   ]
             , v "rv" .->. "canHaveFractionalPart" `assign` (v "fract_flag")
             , v "rv" .->. "canBeNegativeZero" `assign` (v "nz_flag")
             , v "rv" .->. "maxExponent" `assign` (v "exp_set")
             , return_ $ v "rv"
             -- , return_ $ call "optimize" [v "rv"]
             ]
  in Function "Range4" (c "range") args body

range6 :: FunctionDef
range6 =
  let args = [ ("lower_bound", t Signed64)
             , ("has_lower", t Bool)
             , ("upper_bound", t Signed64)
             , ("has_upper", t Bool)
             , ("fract_flag", t Bool)
             , ("nz_flag", t Bool)
             , ("exp_set", t Unsigned16)
             ]
      body = [ declare (c "range") "rv"
             , declare (c "range") "tmp"
             , v "tmp" `assign` v "rv"
             , assert_ $ not_ $ undef $ v "tmp" .->. "lower"
             , assert_ $ not_ $ undef $ v "tmp" .->. "upper"
             , assert_ $ not_ $ undef $ v "rv" .->. "lower"
             , assert_ $ not_ $ undef $ v "rv" .->. "upper"
             , v "rv" `assign` call "setLowerInit" [ v "lower_bound"
                                                   , v "tmp"
                                                   ]
             , v "rv" .->. "hasInt32LowerBound" `assign` v "has_lower"
             , declare (c "range") "tmp2"
             , v "tmp2" `assign` v "rv"
             , v "rv" `assign` call "setUpperInit" [ v "upper_bound"
                                                   , v "tmp2"
                                                   ]
             , v "rv" .->. "hasInt32UpperBound" `assign` v "has_upper"
             , v "rv" .->. "canHaveFractionalPart" `assign` (v "fract_flag")
             , v "rv" .->. "canBeNegativeZero" `assign` (v "nz_flag")
             , v "rv" .->. "maxExponent" `assign` (v "exp_set")
             , return_ $ v "rv"
             -- , return_ $ call "optimize" [v "rv"]
             ]
  in Function "Range6" (c "range") args body

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#394
newInt32Range :: FunctionDef
newInt32Range = let args = [ ("lower_bound_vv", t Signed)
                           , ("upper_bound_vv", t Signed)
                           ]
                    body = [ declare (c "range") "rvvv"
                           , (v "rvvv") .->. "lower" `assign` (v "lower_bound_vv")
                           , (v "rvvv") .->. "upper" `assign` (v "upper_bound_vv")
                           , return_ (v "rvvv")
                           ]
                in Function "newInt32Range" (c "range") args body

optimize :: FunctionDef
optimize =
  let args = [ ("opt_range", c "range") ]
      body = [ declare (c "range") "optrv"
             , v "optrv" `assign` v "opt_range"
             , declare (t Unsigned16) "newExponent"
             , v "newExponent" `assign` n Unsigned16 0

             , if_ (call "hasInt32Bounds" [v "opt_range"])
               [v "newExponent" `assign` (call "exponentImpliedByInt32Bounds" [v "opt_range"])
               , if_ (v "newExponent" .<. (v "opt_range" .->. "maxExponent"))
                 [v "optrv" .->. "maxExponent" `assign` v "newExponent" ] []
               , if_ ((v "opt_range" .->. "canHaveFractionalPart") .&&. ((v "opt_range" .->. "lower") .==. (v "opt_range" .->. "upper")))
                 [v "optrv" .->. "canHaveFractionalPart" `assign` excludesFractionalParts] []
               ] []

             , if_ (v "opt_range" .->. "canBeNegativeZero" .&&. (not_ $ call "canBeZero" [v "opt_range"]))
               [v "optrv" .->. "canBeNegativeZero" `assign` excludesNegativeZero] []

             , return_ $ v "optrv"
             ]
  in Function "optimize" (c "range") args body

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
             ]  -- Do this so it doesnt stamp out previous assign when it gets prev var in if
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

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#566
canHaveSignBitSet :: FunctionDef
canHaveSignBitSet =
  let args = [ ("sbs_range", c "range") ]
      body = [ return_ $ (not_ $ v "sbs_range" .->. "hasInt32LowerBound") .||. (call "canBeFiniteNonNegative" [v "sbs_range"]) .||. (v "sbs_range" .->. "canBeNegativeZero")]
  in Function "canHaveSignBitSet" (t Bool) args body

exponentImpliedByInt32Bounds :: FunctionDef
exponentImpliedByInt32Bounds =
  let args = [ ("eib_range", c "range") ]
      body = [ declare (t Unsigned16) "eib_ret"
             , declare (t Signed) "themax"
             , declare (t Signed) "abs_lower"
             , declare (t Signed) "abs_upper"
             , declare (t Signed) "ua"
             , declare (t Signed) "la"
             , v "la" `assign` (v "eib_range" .->. "lower")
             , v "ua" `assign` (v "eib_range" .->. "upper")
             , v "abs_lower" `assign` (abs_ $ v "la")
             , v "abs_upper" `assign` (abs_ $ v "ua")
             , v "themax" `assign` (cast (max_ (cast (v "abs_lower") Unsigned) (cast (v "abs_upper")Unsigned)) Signed)
             , v "eib_ret" `assign` (fpExp $ cast (v "themax") Double)
             , return_ $ v "eib_ret"
             ]

  in Function "exponentImpliedByInt32Bounds" (t Unsigned16) args body


nullRange :: FunctionDef
nullRange =
  let args = [ ("emptyR", t Bool) ]
      body = [ declare (c "range") "nrRet"
             , v "nrRet" .->. "lower" `assign` jsIntMin
             , v "nrRet" .->. "hasInt32UpperBound" `assign` (n Bool 0)
             , v "nrRet" .->. "upper" `assign` jsIntMin
             , v "nrRet" .->. "hasInt32LowerBound" `assign` (n Bool 0)
             , v "nrRet" .->. "canHaveFractionalPart" `assign` (n Bool 1)
             , v "nrRet" .->. "canBeNegativeZero" `assign` (n Bool 1)
             , v "nrRet" .->. "maxExponent" `assign` includesInfinityAndNan
             , v "nrRet" .->. "isEmpty" `assign` (v "emptyR")
             , return_ $ v "nrRet"
             ]

  in Function "nullRange" (c "range") args body
--- Less complicated stuff

range_constructor :: FunctionDef
range_constructor = undefined

excludesFractionalParts :: Codegen SExpr
excludesFractionalParts = n Bool 0

excludesFractionalPartsS :: String
excludesFractionalPartsS = [i| ((bool) 0) |]

int32min :: Codegen SExpr
int32min = n Signed (-2147483648)

int32minS :: String
int32minS = [i| ((int32_t) -2147483648) |]

int32max :: Codegen SExpr
int32max = n Signed 2147483647

int32maxS :: String
int32maxS = [i| ((int32_t) 2147483647) |]

uint32max :: Codegen SExpr
uint32max = n Unsigned 4294967295

uint32maxS :: String
uint32maxS = [i| ((uint32_t) 4294967295) |]

uint32min :: Codegen SExpr
uint32min = n Unsigned 0

uint32minS :: String
uint32minS = [i| ((uint32_t) 0) |]

excludesNegativeZero :: Codegen SExpr
excludesNegativeZero = n Bool 0

excludesNegativeZeroS :: String
excludesNegativeZeroS = [i| ((bool) 0) |]

maxFiniteExponent :: Codegen SExpr
maxFiniteExponent = n Unsigned16 1023

maxFiniteExponentS :: String
maxFiniteExponentS = [i| ((uint16_t) 1023) |]

includesInfinity :: Codegen SExpr
includesInfinity = n Unsigned16 1

includesInfinityS :: String
includesInfinityS = [i| ((uint16_t) 1 + #{maxFiniteExponentS}) |]

includesInfinityAndNan :: Codegen SExpr
includesInfinityAndNan = n Unsigned16 65535

includesInfinityAndNanS :: String
includesInfinityAndNanS = [i| ((uint16_t) 65535) |]

noInt32LowerBound :: Codegen SExpr
noInt32LowerBound = (cast jsIntMin Signed64) .-. n Signed64 1

noInt32LowerBoundS :: String
noInt32LowerBoundS= [i| ((int64_t) #{jsIntMinS} - (int64_t) 1) |]

noInt32UpperBound :: Codegen SExpr
noInt32UpperBound = (cast jsIntMax Signed64) .+. n Signed64 1

noInt32UpperBoundS :: String
noInt32UpperBoundS= [i| ((int64_t) #{jsIntMaxS} + (int64_t) 1) |]

jsIntMax :: Codegen SExpr
jsIntMax = n Signed (0x7fffffff)

jsIntMaxS :: String
jsIntMaxS = [i| ((int32_t) 0x7fffffff) |]

jsIntMin :: Codegen SExpr
jsIntMin = n Signed (0x80000000)

jsIntMinS :: String
jsIntMinS = [i| ((int32_t) 0x80000000) |]

jsIntMax64 :: Codegen SExpr
jsIntMax64 = n Signed64 2147483647

jsIntMax64S :: String
jsIntMax64S = [i| ((int64_t) 2147483647) |]

jsIntMin64 :: Codegen SExpr
jsIntMin64 = n Signed64 (-2147483648)

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#489
hasInt32Bounds :: FunctionDef
hasInt32Bounds =
  let args = [ ("bnds", c "range") ]
      body = [ return_ $ (v "bnds" .->. "hasInt32LowerBound")  .&&. (v "bnds" .->. "hasInt32UpperBound")
             ]
  in Function "hasInt32Bounds" (t Bool) args body

numBits :: FunctionDef
numBits =
  let args = [ ("nbs", c "range") ]
      body = [ return_ $ ((v "nbs" .->. "maxExponent") .+. n Unsigned16 1)
             ]
  in Function "numBits" (t Unsigned16) args body

canBeFiniteNonNegative :: FunctionDef
canBeFiniteNonNegative =
  let args = [ ("fnn2", c "range") ]
      body = [ return_ $ (v "fnn2" .->. "upper" .=>. n Signed 0) -- finish this
             ]
  in Function "canBeFiniteNonNegative" (t Bool) args body

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

missingAnyInt32Bounds :: FunctionDef
missingAnyInt32Bounds =
  let args = [ ("mibs1", c "range")
             , ("mibs2", c "range")
             ]
      body = [ return_ $ (not_ $ call "hasInt32Bounds" [v "mibs1"]) .||. (not_ $ call "hasInt32Bounds" [v "mibs2"])
             ]
  in Function "missingAnyInt32Bounds" (t Bool) args body

canBeNan :: FunctionDef
canBeNan =
  let args = [ ("nannan", c "range") ]
      body = [ return_ $ (v "nannan" .->. "maxExponent" .==. includesInfinityAndNan)
             ]
  in Function "canBeNan" (t Bool) args body

canBeZero :: FunctionDef
canBeZero =
  let args = [ ("zrange", c "range") ]
      body = [ return_ $ call "contains" [v "zrange", n Signed 0]
             ]
  in Function "canBeZero" (t Bool) args body

contains :: FunctionDef
contains =
  let args = [ ("crange", c "range")
             , ("cval", t Signed)
             ]
      body = [ return_ $ (v "cval" .=>. (v "crange" .->. "lower")) .&&. (v "cval" .<=. (v "crange" .->. "upper"))
             ]
  in Function "contains" (t Bool) args body

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
