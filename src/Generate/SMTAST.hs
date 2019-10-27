module Generate.SMTAST where
import           DSL.Typed

type Version = Int
type VarName = String
type FieldName = String
type ClassName = String

data STy = PrimType Type
         | Class ClassName
         deriving (Eq, Ord, Show)

data SVar = UnknownVersion STy VarName
          | KnownVersion STy VarName Version
         deriving (Eq, Ord, Show)

data SNum = SNum Type Int
          deriving (Eq, Ord, Show)

data SExpr = VarExpr SVar
           | NumExpr SNum
           | GetField SVar FieldName
           deriving (Eq, Ord, Show)

data SStmt = Decl SVar
           | Assign SVar SExpr
           deriving (Eq, Ord, Show)
