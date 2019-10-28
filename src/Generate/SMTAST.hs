module Generate.SMTAST where
import qualified Data.Map  as M
import           DSL.Typed

type Version = Int
type VarName = String
type FieldName = String
type ClassName = String
type FunctionName = String

data STy = PrimType { primTy :: Type }
         | Class    { className :: ClassName }
         deriving (Eq, Ord, Show)

data SVar = SVar { varTy      :: STy
                 , varName    :: VarName
                 , varVersion :: Version
                 }
         deriving (Eq, Ord, Show)

isPrimType :: SVar -> Bool
isPrimType SVar{} = True
isPrimType _      = False

data SNum = SNum Type Int
          deriving (Eq, Ord, Show)

data SExpr = VarExpr SVar
           | NumExpr SNum
           | GetField SVar FieldName
           | Call FunctionName [SExpr]
           deriving (Eq, Ord, Show)

data SStmt = Decl SVar
           | Assign SVar SExpr
           | If SExpr [SStmt] [SStmt]
           | Return SExpr
           deriving (Eq, Ord, Show)

data ClassDef = ClassDef ClassName (M.Map FieldName Type)

