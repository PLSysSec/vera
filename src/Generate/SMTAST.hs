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

setVersion :: SVar -> Int -> SVar
setVersion (SVar ty name _) ver = SVar ty name ver

isPrimType :: SVar -> Bool
isPrimType var = case varTy var of
                   PrimType{} -> True
                   _          -> False

data SNum = SNum { numTy  :: Type
                 , numVal :: Integer
                 }
          deriving (Eq, Ord, Show)

data SExpr = VarExpr { exprVar :: SVar }
           | NumExpr SNum
           | Lt SExpr SExpr
           | Add SExpr SExpr
           | Call FunctionName [SExpr]
           deriving (Eq, Ord, Show)

isClassExpr :: SExpr -> Bool
isClassExpr (VarExpr var) = not $ isPrimType var
isClassExpr _             = False

data SStmt = Decl SVar
           | Assign SVar SExpr
           | AssignField SExpr SExpr
           | If SExpr [SStmt] [SStmt]
           | Return SExpr
           deriving (Eq, Ord, Show)



