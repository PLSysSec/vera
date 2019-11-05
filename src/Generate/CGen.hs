module Generate.CGen where
import Control.Monad
import Generate.SMTAST
import Generate.Lang
import Data.List
import DSL.Typed                  as T
import Lanugage.C.Analysis.SemRep as C

compileSType :: STy -> String
compileSType (PrimType ty) = compileType ty
compileSType (Class name) = name

compileType :: T.Type -> C.TypeName
compileType T.Unsigned = C.TyIntegral C.TyUInt
compileType T.Signed = C.TyIntegral C.Tyint
compileType T.Unsigned64 = C.TyIntegral C.TyULong
compileType T.Signed64 = C.TyIntegral C.TyLong
compileType T.Unsigned16 = C.TyIntegral C.UShort
compileType T.Signed16 = C.TyIntegral C.Short
compileType T.Unsigned8 = C.TyIntegral C.UChar
compileType T.Signed8 = C.TyIntegral C.SChar
compileType T.Bool = C.TyIntegral C.TyBool
compileType T.Double = C.FloatType C.TyDouble

-- TODO: Function definition is broken for now
compileFunctionDef :: FunctionDef -> String
compileFunctionDef (Function funName funTy funArgs funBody) = do
  let retString = compileSType funTy
  let argStringList = map (\(name, ty) -> (compileSType ty) ++ " " ++ name) funArgs
  let argString = intercalate ", " argStringList
  let headerString = retString ++ " " ++ funName ++ "(" ++ argString ++ ") {"
  --letBodyStrings
  "test"

compileSStmt :: SStmt -> [String]
compileSStmt (Decl (SVar varTy varName _)) =
  [(compileType varTy) ++ " " ++ varName]

-- Only works if the left expression is a var?
compileSStmt (Assign (VarExpr (SVar _ varName _ )) expr) =
  [varName ++ " = " ++ (compileSExpr expr)]

-- Possibly don't need
compileSStmt (Assert expr) =
  ["assert(" ++ (compileSExpr expr) ++ ")"]

compileSStmt (If expr thenStmts elseStmts) = do
  let condString = "if(" ++ (compileSExpr expr) ++ ") {"
  let thenStrings = concat $ map compileSStmt thenStmts
  let elseStrings = concat $ map compileSStmt elseStmts
  [condString] ++ (thenStrings ++ elseStrings)

compileSStmt (Return expr) =
  ["return " ++ (compileSExpr expr)]

compileSExpr :: SExpr -> String
compileSExpr (VarExpr (SVar _ varName _)) =
  varName

-- TODO: Right now this only works correctly on integers
compileSExpr (NumExpr (SNum numType numVal)) =
  show numVal

compileSExpr (Neg expr) =
  "-(" ++ (compileSExpr expr) ++ ")"

compileSExpr (Not expr) =
  "!(" ++ (compileSExpr expr) ++ ")"

compileSExpr (Abs expr) =
  "abs(" ++ (compileSExpr expr) ++ ")"

compileSExpr (Eq expr1 expr2) =
  "(" ++ (compileSExpr expr1) ++ ") == (" ++ (compileSExpr expr2) ++ ")"

compileSExpr (And expr1 expr2) =
  "(" ++ (compileSExpr expr1) ++ ") & (" ++ (compileSExpr expr2) ++ ")"

compileSExpr (Add expr1 expr2) =
  "(" ++ (compileSExpr expr1) ++ ") + (" ++ (compileSExpr expr2) ++ ")"

compileSExpr (Sub expr1 expr2) =
  "(" ++ (compileSExpr expr1) ++ ") - (" ++ (compileSExpr expr2) ++ ")"

compileSExpr (Mul expr1 expr2) =
  "(" ++ (compileSExpr expr1) ++ ") * (" ++ (compileSExpr expr2) ++ ")"

compileSExpr (Or expr1 expr2) =
  "(" ++ (compileSExpr expr1) ++ ") | (" ++ (compileSExpr expr2) ++ ")"

compileSExpr (XOr expr1 expr2) =
  "(" ++ (compileSExpr expr1) ++ ") ^ (" ++ (compileSExpr expr2) ++ ")"

compileSExpr (XOr expr1 expr2) =
  "(" ++ (compileSExpr expr1) ++ ") ^ (" ++ (compileSExpr expr2) ++ ")"

compileSExpr (Min expr1 expr2) =
  "min((" ++ (compileSExpr expr1) ++ "), (" ++ (compileSExpr expr2) ++ "))"

compileSExpr (Max expr1 expr2) =
  "max((" ++ (compileSExpr expr1) ++ "), (" ++ (compileSExpr expr2) ++ "))"

compileSExpr (Gt expr1 expr2) =
  "(" ++ (compileSExpr expr1) ++ ") > (" ++ (compileSExpr expr2) ++ ")"

compileSExpr (Gte expr1 expr2) =
  "(" ++ (compileSExpr expr1) ++ ") >= (" ++ (compileSExpr expr2) ++ ")"

compileSExpr (Lt expr1 expr2) =
  "(" ++ (compileSExpr expr1) ++ ") < (" ++ (compileSExpr expr2) ++ ")"

compileSExpr (Lte expr1 expr2) =
  "(" ++ (compileSExpr expr1) ++ ") <= (" ++ (compileSExpr expr2) ++ ")"

compileSExpr (Shl expr1 expr2) =
  "(" ++ (compileSExpr expr1) ++ ") << (" ++ (compileSExpr expr2) ++ ")"

compileSExpr (Shr expr1 expr2) =
  "(" ++ (compileSExpr expr1) ++ ") >> (" ++ (compileSExpr expr2) ++ ")"

compileSExpr (Cast expr ty) =
  "(" ++ (compileType ty) ++ ")(" ++ (compileSExpr expr) ++ ")"

compileSExpr (Call name exprs) = do
  let exprStrings = map compileSExpr exprs
  let argString = intercalate ", " exprStrings
  name ++ "(" ++ argString ++ ")"

compileSExpr (FieldExpr name) =
  "this." ++ name

