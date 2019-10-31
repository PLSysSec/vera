module IonMonkeyGenerated.Objects where
import           DSL.Typed     (Type (..))
import           Generate.Lang

-- | Set up invariants for our input range. This is not an IonMonkey function
newInt32InputRange :: FunctionDef
newInt32InputRange =
  let body = [ declare (c "range") "rv"
             , (v "rv") .->. "hasInt32LowerBound" `assign` (n Bool 1)
             , (v "rv") .->. "hasInt32UpperBound" `assign` (n Bool 1)
             , assert_ $ ((v "rv") .->. "lower") .<. ((v "rv") .->. "upper")
             , return_ $ v "rv"
             ]
  in Function "newIn32InputRange" (c "range") [] body

-- | The IonMonkey range object
range :: ClassDef
range = let fields = [ ("lower", Signed)
                     , ("upper", Signed)
                     , ("hasInt32LowerBound", Bool)
                     , ("hasInt32UpperBound", Bool)
                     , ("canBeNegativeZero", Bool)
                     , ("maxExponent", Unsigned16)
                     ]
        in ClassDef "range" fields []
