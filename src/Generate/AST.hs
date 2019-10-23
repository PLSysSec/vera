module Generate.AST where
import           Control.Monad  (unless)
import qualified DSL.DSL        as D
import           DSL.Typed
import           Generate.State

data Expr = Add VNode VNode
          | Sub VNode VNode
          | Mul VNode VNode
          | Cast VNode Type
          | Max VNode VNode
          | Min VNode VNode
          -- ^ Operators
          | Lt VNode VNode
          | Lte VNode VNode
          | Gt VNode VNode
          | Gte VNode VNode
          -- ^ Comparisons
          | And VNode VNode
          | Or VNode VNode
          | Xor VNode VNode
          | LShift VNode VNode
          | RShift VNode VNode
          -- ^ Bitwise operators
          | Abs VNode
          | Not VNode
          | Neg VNode
          -- ^ Unary operators
          deriving (Eq, Ord, Show)



_elif = undefined

_else = undefined

_return = undefined

--

eq = undefined

(+) = undefined

(-) = undefined

(||) = undefined

(!) = undefined

(++) = undefined

(&&) = undefined

--

_call = undefined

(-->) = undefined

(.) = undefined






