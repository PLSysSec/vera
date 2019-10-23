module Generate.AST where
import           Control.Monad  (unless)
import qualified DSL.DSL        as D
import           DSL.Typed
import           Generate.State

data RichType = Type
              | Class String
              deriving (Eq, Ord, Show)

data Leaf = VNode
          | Member String
          | Field String

data Expr = Add RichType Expr Expr
          | Sub RichType Expr Expr
          | Mul RichType Expr Expr
          | Cast RichType Expr Expr
          | Max RichType Expr Expr
          | Min RichType Expr Expr
          | GetField RichType Expr Expr
          -- ^ Operators
          | Lt RichType Expr Expr
          | Lte RichType Expr Expr
          | Gt RichType Expr Expr
          | Gte RichType Expr Expr
          | Eq RichType Expr Expr
          -- ^ Comparisons
          | And RichType Expr Expr
          | Or RichType Expr Expr
          | Xor RichType Expr Expr
          | LShift RichType Expr Expr
          | RShift RichType Expr Expr
          -- ^ Bitwise operators
          | Abs RichType Expr
          | Not RichType Expr
          | Neg RichType Expr
          | Leaf
          -- ^ Unary operators
          deriving (Eq, Ord, Show)

data Stmt = Assign VNode Expr
          | If VNode [Stmt] (Maybe [Stmt])
          | Return VNode
          | Call RichType String [Expr]
          deriving (Eq, Ord, Show)

data FieldDef = FieldDef RichType String
data MemberDef = MemberDef Function
data ClassDef = ClassDef [FieldDef] [MemberDef]

data Function = Function String RichType [RichType]
data Program = Program [Function] [ClassDef]


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






