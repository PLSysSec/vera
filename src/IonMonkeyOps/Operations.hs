module IonMonkeyOps.Operations () where
import qualified DSL.DSL                       as D
import           IonMonkeyOps.IonMonkeyObjects

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#744
add :: (D.MonadBoolector m) => m Range
add = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#775
sub :: (D.MonadBoolector m) => m Range
sub = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#805
and :: (D.MonadBoolector m) => m Range
and = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#834
or :: (D.MonadBoolector m) => m Range
or = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#893
xor :: (D.MonadBoolector m) => m Range
xor = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#955
not :: (D.MonadBoolector m) => m Range
not = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#960
mul :: (D.MonadBoolector m) => m Range
mul = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#999
lsh :: (D.MonadBoolector m) => m Range
lsh = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1016
rsh :: (D.MonadBoolector m) => m Range
rsh = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1023
ursh :: (D.MonadBoolector m) => m Range
ursh = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1042
lsh' :: (D.MonadBoolector m) => m Range
lsh' = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1048
rsh' :: (D.MonadBoolector m) => m Range
rsh' = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1079
ursh' :: (D.MonadBoolector m) => m Range
ursh' = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1089
abs :: (D.MonadBoolector m) => m Range
abs = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1104
min :: (D.MonadBoolector m) => m Range
min = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#1123
max :: (D.MonadBoolector m) => m Range
max = undefined





