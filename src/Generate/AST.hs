module Generate.AST where
import           Control.Monad  (unless)
import qualified DSL.DSL        as D
import           DSL.Typed
import           Generate.State

data RichType = Normal Type
              | Class String
              deriving (Eq, Ord, Show)

data Leaf = VNode
          | Member String
          | Field String

data Expr = Add { ty    :: RichType
                , left  :: Expr
                , right :: Expr
                }
          | Sub { ty    :: RichType
                , left  :: Expr
                , right :: Expr
                }
          | Mul { ty    :: RichType
                , left  :: Expr
                , right :: Expr
                }
          | Cast { ty   :: RichType
                 , expr :: Expr
                 }
          | Max { ty    :: RichType
                , left  :: Expr
                , right :: Expr
                }
          | Min { ty    :: RichType
                , left  :: Expr
                , right :: Expr
                }
          | GetField { ty    :: RichType
                     , left  :: Expr
                     , right :: Expr
                     }
          -- ^ Operators
          | Lt { ty    :: RichType
               , left  :: Expr
               , right :: Expr
               }
          | Lte { ty    :: RichType
                , left  :: Expr
                , right :: Expr
                }
          | Gt { ty    :: RichType
               , left  :: Expr
               , right :: Expr
               }
          | Gte { ty    :: RichType
                , left  :: Expr
                , right :: Expr
                }
          | Eq { ty    :: RichType
               , left  :: Expr
               , right :: Expr
               }
          -- ^ Comparisons
          | And { ty    :: RichType
                , left  :: Expr
                , right :: Expr
                }
          | Or { ty    :: RichType
               , left  :: Expr
               , right :: Expr
               }
          | Xor { ty    :: RichType
                , left  :: Expr
                , right :: Expr
                }
          | LShift { ty    :: RichType
                   , left  :: Expr
                   , right :: Expr
                   }
          | RShift { ty    :: RichType
                   , left  :: Expr
                   , right :: Expr
                   }
          -- ^ Bitwise operators
          | Abs { ty   :: RichType
                , expr :: Expr
                }
          | Not { ty   :: RichType
                , expr :: Expr
                }
          | Neg { ty   :: RichType
                , expr :: Expr
                }
          | Leaf
          -- ^ Unary operators
          deriving (Eq, Ord, Show)

data Stmt = Assign VNode Expr
          | If Expr [Stmt] (Maybe [Stmt])
          | Return VNode
          | Call RichType String [Expr]
          deriving (Eq, Ord, Show)

data FieldDef = FieldDef RichType String
data MemberDef = MemberDef Function
data ClassDef = ClassDef [FieldDef] [MemberDef]

data Function = Function String RichType [RichType]
data Program = Program [Function] [ClassDef]

