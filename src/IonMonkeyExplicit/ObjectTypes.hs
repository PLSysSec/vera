module IonMonkeyExplicit.ObjectTypes (Range(..)) where
import           DSL.Typed as T

-- | IonMonkey's range object
-- https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.h#119
data Range = Range {
      rangeName             :: String
    , lower                 :: T.VNode
    , upper                 :: T.VNode
    , hasInt32LowerBound    :: T.VNode
    , hasInt32UpperBound    :: T.VNode
    , canBeNegativeZero     :: T.VNode
    , canHaveFractionalPart :: T.VNode
    , maxExponent           :: T.VNode
    }

