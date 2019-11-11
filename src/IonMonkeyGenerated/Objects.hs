module IonMonkeyGenerated.Objects where
import           DSL.Typed     (Type (..))
import           Generate.Lang

-- | Set up invariants for our input range. This is not an IonMonkey function
newInt32InputRange :: FunctionDef
newInt32InputRange =
  let body = [ declare (c "range") "rv"
             , (v "rv") .->. "hasInt32LowerBound" `assign` (n Bool 1)
             , (v "rv") .->. "hasInt32UpperBound" `assign` (n Bool 1)
             , assert_ $ ((v "rv") .->. "lower") .<=. ((v "rv") .->. "upper")
             , return_ $ v "rv"
             ]
  in Function "newInt32InputRange" (c "range") [] body

newFloatInputRange :: FunctionDef
newFloatInputRange =
  -- // To facilitate this trick, we maintain the invariants that:
  -- // 1) hasInt32LowerBound_ == false implies lower_ == JSVAL_INT_MIN
  -- // 2) hasInt32UpperBound_ == false implies upper_ == JSVAL_INT_MAX
  let body = [ declare (c "range") "rv"
             , assert_ $ ((v "rv") .->. "lower") .<=. ((v "rv") .->. "upper")
             , assert_ $ (((v "rv" .->. "lower") .==. n Signed (-2147483648)) .||. (v "rv" .->. "hasInt32LowerBound"))
             , assert_ $ (((v "rv" .->. "upper") .==. n Signed 2147483647) .||. (v "rv" .->. "hasInt32LowerBound"))
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
