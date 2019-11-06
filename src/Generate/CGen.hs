module Generate.CGen where
import Control.Monad
import Generate.SMTAST
import Generate.Lang
import Data.List
import DSL.Typed       as T

compileSType :: STy -> String
compileSType (PrimType ty) = compileType ty
compileSType (Class name) = name
compileSType void = "void"

compileType :: T.Type -> String
compileType T.Unsigned = "uint32_t"
compileType T.Signed = "int32_t"
compileType T.Unsigned64 = "uint64_t"
compileType T.Signed64 = "int64_t"
compileType T.Unsigned16 = "uint16_t"
compileType T.Signed16 = "int16_t"
compileType T.Unsigned8 = "uint8_t"
compileType T.Signed8 = "int8_t"
compileType T.Double = "double"
compileType T.Bool = "bool"

compileParams :: [(VarName, STy)] -> String
compileParams params = do
  let paramStrings = map (\(name, ty) -> (compileSType ty) ++ " " ++ name) params
  intercalate ", " paramStrings

compileSFunction :: SFunction -> [String]
compileSFunction (SFunction name ty params body) = do
  let paramString = compileParams params
  let headerString = (compileSType ty) ++ name ++ "(" ++ paramString ++ ") {"
  let bodyStrings = concat $ map compileSStmt body
  [headerString] ++ bodyStrings ++ ["}"]

compileSClass :: SClass -> [String]
compileSClass (SClass name fields methods) = do
  let headerStrings = ["class " ++ name ++ " {", "public:"]
  let fieldStrings = map (\(argName, ty) -> (compileSType ty) ++ " " ++ argName ++ ";") fields
  let methodStrings = concat $ map compileSFunction methods
  headerStrings ++ fieldStrings ++ methodStrings ++ ["};"]

compileSStmt :: SStmt -> [String]
compileSStmt (Decl (SVar ty name _)) =
  [(compileType ty) ++ " " ++ name ++ ";"]

compileSStmt (Decl (CVar className name)) =
  [className ++ " " ++ name ++ ";"]

-- Only works if the left expression is a var?
compileSStmt (Assign expr1 expr2) =
  [(compileSExpr expr1) ++ " = " ++ (compileSExpr expr2) ++ ";"]

-- Possibly don't need
compileSStmt (Assert expr) =
  ["assert(" ++ (compileSExpr expr) ++ ")" ++ ";"]

compileSStmt (If expr thenStmts elseStmts) = do
  let condString = "if(" ++ (compileSExpr expr) ++ ") {"
  let thenStrings = concat $ map compileSStmt thenStmts
  let elseStrings = concat $ map compileSStmt elseStmts
  [condString] ++ (thenStrings ++ elseStrings)

compileSStmt (Return expr) =
  ["return " ++ (compileSExpr expr) ++ ";"]

compileSStmt (VoidCall name args) =
  [(compileSExpr (Call name args)) ++ ";"]

compileSExpr :: SExpr -> String
compileSExpr (VarExpr (SVar _ name _)) =
  name

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

