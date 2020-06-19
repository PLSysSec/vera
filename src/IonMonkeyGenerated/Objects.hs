module IonMonkeyGenerated.Objects where
import           DSL.Typed     (Type (..))
import           Generate.Lang

-- | Set up invariants for our input range. This is not an IonMonkey function
newInt32InputRange :: FunctionDef
newInt32InputRange =
  let body = [ declare (c "range") "rvir"
             , (v "rvir") .->. "hasInt32LowerBound" `assign` (n Bool 1)
             , (v "rvir") .->. "hasInt32UpperBound" `assign` (n Bool 1)
             , (v "rvir") .->. "canHaveFractionalPart" `assign` (n Bool 1)
             , return_ $ v "rvir"
             ]
  in Function "newInt32InputRange" (c "range") [] body

newFloatInputRange :: FunctionDef
newFloatInputRange =
  let body = [ declare (c "range") "rvfl" ]
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
