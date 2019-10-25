module Generate.AST where
import           Control.Monad (unless)
import qualified DSL.DSL       as D
import           DSL.Typed

type Version = Int
type Variable = String

data RichType = Normal { ntype :: Type }
              | Class String
              deriving (Eq, Ord, Show)

data Leaf = V Variable
          | VV VNode Variable Version
          | N VNode
          | Member String
          | Field String
          deriving (Eq, Ord, Show)

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
          | Simple Leaf
          -- ^ Unary operators
          deriving (Eq, Ord, Show)

data Stmt = Assign Expr Expr
          | If Expr [Stmt] [Stmt]
          | Decl Variable Type
          | Return VNode
          | Call RichType String [Expr]
          deriving (Eq, Ord, Show)

data FieldDef = FieldDef RichType String
data MemberDef = MemberDef Function
data ClassDef = ClassDef [FieldDef] [MemberDef]

data Function = Function { funName :: String
                         , funType :: Type
                         , funArgs :: [Type]
                         , funBody :: [Stmt]
                         }

data Program = Program [Function] [ClassDef]

