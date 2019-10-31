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

isClass :: STy -> Bool
isClass Class{} = True
isClass _       = False

data SVar = SVar { varTy      :: Type
                 , varName    :: VarName
                 , varVersion :: Version
                 }
          | CVar { varClass :: ClassName
                 , varName  :: VarName
                 }
         deriving (Eq, Ord, Show)

isPrimType :: SVar -> Bool
isPrimType SVar{} = True
isPrimType _      = False

isClassType :: SVar -> Bool
isClassType = not . isPrimType

-- Class var is an option, it just come with class name and no version
-- let assign be expr -> expr now
-- get rid of assign field. assign will break things down by field
-- change the state map bakc to just types?

setVersion :: SVar -> Int -> SVar
setVersion (SVar ty name _) ver = SVar ty name ver
setVersion cv _                 = cv

data SNum = SNum { numTy  :: Type
                 , numVal :: Integer
                 }
          deriving (Eq, Ord, Show)

data SExpr = VarExpr { exprVar :: SVar }
           | NumExpr SNum
           | Neg SExpr
           | Not SExpr
           | Abs SExpr
           | Eq SExpr SExpr
           | And SExpr SExpr
           | Add SExpr SExpr
           | Sub SExpr SExpr
           | Mul SExpr SExpr
           | Or SExpr SExpr
           | XOr SExpr SExpr
           | Min SExpr SExpr
           | Max SExpr SExpr
           | Gt SExpr SExpr
           | Gte SExpr SExpr
           | Lt SExpr SExpr
           | Lte SExpr SExpr
           | Shl SExpr SExpr
           | Shr SExpr SExpr
           | Call FunctionName [SExpr]
           | FieldExpr FieldName
           deriving (Eq, Ord, Show)

isCallExpr :: SExpr -> Bool
isCallExpr Call{} = True
isCallExpr _      = False

isClassExpr :: SExpr -> Bool
isClassExpr (VarExpr v) = not $ isPrimType v
isClassExpr _           = False

isPrimVarExpr :: SExpr -> Bool
isPrimVarExpr (VarExpr v) = isPrimType v
isPrimVarExpr _           = False

data SStmt = Decl SVar
           | Assign SExpr SExpr
           | Assert SExpr
           | If SExpr [SStmt] [SStmt]
           | Return SExpr
           deriving (Eq, Ord, Show)


