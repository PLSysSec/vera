module IonMonkeyGenerated.Bugs ( badMod
                               , goodMod
                               , badIntersect
                               ) where
import           Control.Monad
import           DSL.Typed                  (Type (..))
import           Generate.Lang
import           IonMonkeyGenerated.Helpers
import           Prelude                    hiding (abs, and, floor, max, min,
                                             not, or)

badMod :: FunctionDef
badMod =
  let args = [ ("lhs", c "range")
             , ("rhs", c "range")
             ]
      body = [ declare (c "range") "a"
             , declare (c "range") "b"
             , declare (c "range") "bound"
             , v "a" `assign` (abs_ (cast (v "rhs" .->. "lower") Signed64))
             , v "b" `assign` (abs_ (cast (v "rhs" .->. "upper") Signed64))
             , if_ ((v "a" .==. n Signed64 0) .&&. (v "b" .==. n Signed64 0))
               [return_ $ call "newInt32Range" [int32min, int32max]] []
             , v "bound" `assign` max_ (n Signed64 1 .-. v "a") (v "b" .-. n Signed64 1)
             , return_ $ call "newInt32Range" [neg_ $ v "bound", v "bound"]
             ]
  in Function "badMod" (c "range") args body

goodMod :: FunctionDef
goodMod =
  let args = [ ("lhs", c "range")
             , ("rhs", c "range")
             ]
      body = [ declare (c "range") "a"
             , declare (c "range") "b"
             , declare (c "range") "bound"
             , v "a" `assign` (cast (v "rhs" .->. "lower") Signed64)
             , v "b" `assign` (cast (v "rhs" .->. "upper") Signed64)
             , if_ ((v "a" .==. n Signed64 0) .&&. (v "b" .==. n Signed64 0))
               [return_ $ call "newInt32Range" [int32min, int32max]] []
             , v "bound" `assign` max_ (abs_ $ n Signed64 1 .-. v "a") (abs_ $ v "b" .-. n Signed64 1)
             , return_ $ call "newInt32Range" [neg_ $ v "bound", v "bound"]
             ]
  in Function "badMod" (c "range") args body


badIntersect :: FunctionDef
badIntersect =
  let args = []
      body = []
  in Function "badMod" (c "range") args body



