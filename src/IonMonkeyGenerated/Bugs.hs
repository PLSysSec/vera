module IonMonkeyGenerated.Bugs ( badMod32
                               , goodMod32
                               , badIntersect
                               ) where
import           Control.Monad
import           DSL.Typed                  (Type (..))
import           Generate.Lang
import           IonMonkeyGenerated.Helpers
import           Prelude                    hiding (abs, and, floor, max, min,
                                             not, or)

badMod32 :: FunctionDef
badMod32 =
  let args = [ ("lhs", c "range")
             , ("rhs", c "range")
             ]
      body = [ declare (t Signed64) "a"
             , declare (t Signed64) "b"
             , declare (t Signed64) "bound"
             , v "a" `assign` (abs_ (cast (v "rhs" .->. "lower") Signed64))
             , v "b" `assign` (abs_ (cast (v "rhs" .->. "upper") Signed64))
             , if_ ((v "a" .==. n Signed64 0) .&&. (v "b" .==. n Signed64 0))
               [return_ $ call "newInt32Range" [int32min, int32max]] []
             , v "bound" `assign` max_ (n Signed64 1 .-. v "a") (v "b" .-. n Signed64 1)
             , return_ $ call "newInt32Range" [cast (neg_ $ v "bound") Signed, cast (v "bound") Signed]
             ]
  in Function "badMod32" (c "range") args body

goodMod32 :: FunctionDef
goodMod32 =
  let args = [ ("lhs", c "range")
             , ("rhs", c "range")
             ]
      body = [ declare (t Signed64) "a"
             , declare (t Signed64) "b"
             , declare (t Signed64) "bound"
             , v "a" `assign` (cast (v "rhs" .->. "lower") Signed64)
             , v "b" `assign` (cast (v "rhs" .->. "upper") Signed64)
             , if_ ((v "a" .==. n Signed64 0) .&&. (v "b" .==. n Signed64 0))
               [return_ $ call "newInt32Range" [int32min, int32max]] []
             , v "bound" `assign` max_ (abs_ $ n Signed64 1 .-. v "a") (abs_ $ v "b" .-. n Signed64 1)
             , return_ $ call "newInt32Range" [cast (neg_ $ v "bound") Signed, cast (v "bound") Signed]
             ]
  in Function "goodMod32" (c "range") args body


badIntersect :: FunctionDef
badIntersect =
  let args = []
      body = []
  in Function "badMod" (c "range") args body



