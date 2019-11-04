module Generate.CGen where
import Generate.Lang
import Data.List

compileSType :: STy -> String
compileSType (PrimType ty) = compileType ty
compileSType Class name = name

compileType :: Type -> String
compileType Unsigned = "uint32_t"
compileType Signed = "int32_t"
compileType Unsigned64 = "uint64_t"
compileType Signed64 = "int64_t"
compileType Unsigned16 = "uint16_t"
compileType Signed16 = "int16_t"
compileType Unsigned8 = "uint8_t"
compileType Signed8 = "int8_t"
compileType Double = "double"
compileType Bool = "bool"

compileFunctionDef :: FunctionDef -> String
compileFunctionDef (Function funName funTy funArgs funBody) = do
  let retString = compileType funTy
  let argStringList = forM_ funArgs $ \(name, ty) -> (compileType ty) ++ " " ++ name
  let argString = intercalate ", " argStringList
  let headerString = retString ++ " " ++ funName ++ "(" ++ argString ++ ") {"

compileSStmt :: SStmt -> String
compileSStmt Decl (SVar varTy varName _) =
  (compileType varTy) ++ " " ++ varName

-- Only works if the left expression is a var?
compileSStmt (Assign (VarExpr (SVar _ varName _ )) expr) =
  varName ++ " = " ++ (compileSStmt expr)

-- Possibly don't need
compileSStmt (Assert expr) =
  "assert(" ++ (compileSStmt expr) ++ ")"

compileSStmt (If expr thenStmts elseStmts) = do
  let condString = "if(" ++ (compileSStmt) expr ++ ") {"
  let thenStrings = forM_ thenStmts compileSStmt
  let elseStrings = forM_ elseStmts compileSStmt
  [condString] ++ thenStrings ++ elseSTrings

compileSStmt (Return expr) =
  "return " ++ (compileSExpr expr)

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

compileSExpr (Abs SExpr) =
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
  let exprStrings = forM_ exprs compileSExpr
  let argString = intercalate ", " exprStrings
  name ++ "(" ++ exprStrings ++ ")"

compileSExpr (FieldExpr name) =
  "this." ++ name

