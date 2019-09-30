module DSL.Typed where
import qualified DSL.DSL as D

{-|

Typed wrappers around C++ operations so that we don't have to case them out by hand.
Relying on cppreference.com for spec for each operator

-}

cppCompareWrapper :: D.VNode
                  -> D.VNode
                  -> (D.Node -> D.Node -> D.Verif D.Node)
                  -> (D.Node -> D.Node -> D.Verif D.Node)
                  -> D.Verif D.VNode
cppCompareWrapper left right uCompare sCompare
 | D.isUnsigned (D.vtype left) || D.isUnsigned (D.vtype right) = do
     compare <- uCompare (D.vnode left) (D.vnode right)
     return $ D.VNode compare D.Unsigned
 | otherwise = do
     compare <- sCompare (D.vnode left) (D.vnode right)
     return $ D.VNode compare D.Unsigned

cppGt :: D.VNode -> D.VNode -> D.Verif D.VNode
cppGt left right = cppCompareWrapper left right D.ugt D.sgt

cppGte :: D.VNode -> D.VNode -> D.Verif D.VNode
cppGte left right = cppCompareWrapper left right D.ugte D.sgte

cppLt :: D.VNode -> D.VNode -> D.Verif D.VNode
cppLt left right = cppCompareWrapper left right D.ult D.slt

cppLte :: D.VNode -> D.VNode -> D.Verif D.VNode
cppLte left right = cppCompareWrapper left right D.ulte D.slte

cppShiftLeft :: D.VNode -> D.VNode -> D.Verif D.VNode
cppShiftLeft left right = undefined

-- |
-- https://en.cppreference.com/w/cpp/language/operator_arithmetic
--
-- For unsigned a and for signed and non-negative a, the value of a >> b is the integer
-- part of a/2b
--
-- For negative a, the value of a >> b is implementation-defined
-- (in most implementations, this performs arithmetic right shift,
-- so that the result remains negative).
--
-- The value of a >> b is a/2b, rounded down (in other words, right shift on
-- signed a is arithmetic right shift).
--
cppShiftRight :: D.VNode -> D.VNode -> D.Verif D.VNode
cppShiftRight left right = undefined
 -- | D.isUnsigned left && D.isUnsigned right = undefined
 -- | otherwise = undefined

cppAdd :: D.VNode -> D.VNode -> D.Verif D.VNode
cppAdd = undefined

cppOr :: D.VNode -> D.VNode -> D.Verif D.VNode
cppOr = undefined
