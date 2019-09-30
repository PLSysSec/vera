module DSL.Typed where
import           DSL.DSL

{-|

Typed wrappers around C++ operations so that we don't have to case them out by hand

-}

cppGt :: VNode -> VNode -> Verif VNode
cppGt = undefined

cppGte :: VNode -> VNode -> Verif VNode
cppGte = undefined

cppLt :: VNode -> VNode -> Verif VNode
cppLt = undefined

cppLte :: VNode -> VNode -> Verif VNode
cppLte = undefined

cppShiftLeft :: VNode -> VNode -> Verif VNode
cppShiftLeft left right = undefined

cppShiftRight :: VNode -> VNode -> Verif VNode
cppShiftRight left right = undefined

cppAdd :: VNode -> VNode -> Verif VNode
cppAdd = undefined

cppOr :: VNode -> VNode -> Verif VNode
cppOr = undefined
