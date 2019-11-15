module IonMonkeyGenerated.Objects where
import           DSL.Typed     (Type (..))
import           Generate.Lang

-- | Set up invariants for our input range. This is not an IonMonkey function
newInt32InputRange :: FunctionDef
newInt32InputRange =
  let body = [ declare (c "range") "rvir"
             , (v "rvir") .->. "hasInt32LowerBound" `assign` (n Bool 1)
             , assert_ $ not_ $ undef $ v "rvir" .->. "hasInt32LowerBound"
             , (v "rvir") .->. "hasInt32UpperBound" `assign` (n Bool 1)
             , (v "rvir") .->. "canHaveFractionalPart" `assign` (n Bool 1)
             , assert_ $ not_ $ undef $ v "rvir" .->. "hasInt32UpperBound"
             , assert_ $ ((v "rvir") .->. "lower") .<=. ((v "rvir") .->. "upper")
             , return_ $ v "rvir"
             ]
  in Function "newInt32InputRange" (c "range") [] body

newFloatInputRange :: FunctionDef
newFloatInputRange =
  -- // To facilitate this trick, we maintain the invariants that:
  -- // 1) hasInt32LowerBound_ == false implies lower_ == JSVAL_INT_MIN
  -- // 2) hasInt32UpperBound_ == false implies upper_ == JSVAL_INT_MAX
  let body = [ declare (c "range") "rvfl"
             , assert_ $ ((v "rvfl") .->. "lower") .<=. ((v "rvfl") .->. "upper")
             , assert_ $ (((v "rvfl" .->. "lower") .==. n Signed (-2147483648)) .||. (v "rvfl" .->. "hasInt32LowerBound"))
             , assert_ $ (((v "rvfl" .->. "upper") .==. n Signed 2147483647) .||. (v "rvfl" .->. "hasInt32LowerBound"))
             ]
  in Function "newFloatInputRange" (c "range") [] body

-- | The IonMonkey range object
range :: ClassDef
range = let fields = [ ("lower", Signed)
                     , ("upper", Signed)
                     , ("hasInt32LowerBound", Bool)
                     , ("hasInt32UpperBound", Bool)
                     , ("canHaveFractionalPart", Bool)
                     , ("canBeNegativeZero", Bool)
                     , ("maxExponent", Unsigned16)
                     , ("isEmpty", Bool)
                     ]
        in ClassDef "range" fields []
