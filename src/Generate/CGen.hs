module Generate.CGen where
import Control.Monad
import Generate.SMTAST
import Generate.State
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

compileFunction :: FunctionDef -> Codegen [String]
compileFunction (Function name ty params body) = do
  let paramString = compileParams params
      headerString = (compileSType ty) ++ " " ++ name ++ "(" ++ paramString ++ ") {"
  bodyCodegenStrings <- mapM compileCodegenSStmt body
  return $ [headerString] ++ bodyCodegenStrings ++ ["}", ""]

compileCodegenSStmt :: Codegen SStmt -> Codegen String
compileCodegenSStmt stmt = do
  s <- stmt
  return $ unlines $ compileSStmt s

compileSFunction :: SFunction -> [String]
compileSFunction (SFunction name ty params body) = do
  let paramString = compileParams params
  let headerString = (compileSType ty) ++ " " ++ name ++ "(" ++ paramString ++ ") {"
  let bodyStrings = concat $ map compileSStmt body
  [headerString] ++ bodyStrings ++ ["}"]

compileSClass :: SClass -> [String]
compileSClass (SClass name fields methods) = do
  let headerStrings = ["class " ++ name ++ " {", "public:"]
  let fieldStrings = map (\(argName, ty) -> (compileSType ty) ++ " " ++ argName ++ ";") fields
  let methodStrings = concat $ map compileSFunction methods
  headerStrings ++ fieldStrings ++ methodStrings ++ ["};"]

compileSStmt :: SStmt -> [String]
compileSStmt stmt = case stmt of
  (Decl (SVar ty name _)) -> [(compileType ty) ++ " " ++ name ++ ";"]
  (Decl var) -> case var of
    SVar ty name _ -> [(compileType ty) ++ " " ++ name ++ ";"]
    CVar cl name   -> [cl ++ " " ++ name ++ ";"]
  (Assign expr1 expr2) -> [(compileSExpr expr1) ++ " = " ++ (compileSExpr expr2) ++ ";"]
  (AddEq _ expr1 expr2) -> [c1 ++ " += " ++ c2 ++ ";"] where
    c1 = compileSExpr expr1
    c2 = compileSExpr expr2
  (SubEq _ expr1 expr2) -> [c1 ++ " -= " ++ c2 ++ ";"] where
    c1 = compileSExpr expr1
    c2 = compileSExpr expr2
  (OrEq _ expr1 expr2) -> [c1 ++ " |= " ++ c2 ++ ";"] where
    c1 = compileSExpr expr1
    c2 = compileSExpr expr2
  (AndEq _ expr1 expr2) -> [c1 ++ " &= " ++ c2 ++ ";"] where
    c1 = compileSExpr expr1
    c2 = compileSExpr expr2
  Push -> [""]
  Pop -> [""]
  (Assert expr) -> ["assert(" ++ (compileSExpr expr) ++ ")" ++ ";"]
  (If expr thenStmts elseStmts) -> [condString] ++ thenStrings ++ elseConcatString ++ elseStrings ++ ["}"] where
    condString = "if(" ++ (compileSExpr expr) ++ ") {"
    thenStrings = concatMap compileSStmt thenStmts
    elseStrings = concatMap compileSStmt elseStmts
    elseConcatString = if (null elseStrings)
                            then [""]
                            else ["} else {"]
  (Return expr) -> ["return " ++ (compileSExpr expr) ++ ";"]
  (VoidCall name args) -> [(compileSExpr (Call name args)) ++ ";"]

compileSExpr :: SExpr -> String
compileSExpr expr = case expr of
  (VarExpr var) ->
    case var of
      SVar _ name _ -> name
      CVar _ name   -> name
  (NumExpr (SNum numType numVal)) -> show numVal
  (Neg expr) -> "-(" ++ (compileSExpr expr) ++ ")"
  (Not expr) -> "!(" ++ (compileSExpr expr) ++ ")"
  (Abs expr) -> "abs(" ++ (compileSExpr expr) ++ ")"
  (Eq expr1 expr2) -> "(" ++ (compileSExpr expr1) ++ ") == (" ++ (compileSExpr expr2) ++ ")"
  (And expr1 expr2) -> "(" ++ (compileSExpr expr1) ++ ") & (" ++ (compileSExpr expr2) ++ ")"
  (Add expr1 expr2) -> "(" ++ (compileSExpr expr1) ++ ") + (" ++ (compileSExpr expr2) ++ ")"
  (Sub expr1 expr2) -> "(" ++ (compileSExpr expr1) ++ ") - (" ++ (compileSExpr expr2) ++ ")"
  (Mul expr1 expr2) -> "(" ++ (compileSExpr expr1) ++ ") * (" ++ (compileSExpr expr2) ++ ")"
  (Or expr1 expr2) -> "(" ++ (compileSExpr expr1) ++ ") | (" ++ (compileSExpr expr2) ++ ")"
  (XOr expr1 expr2) -> "(" ++ (compileSExpr expr1) ++ ") ^ (" ++ (compileSExpr expr2) ++ ")"
  (XOr expr1 expr2) -> "(" ++ (compileSExpr expr1) ++ ") ^ (" ++ (compileSExpr expr2) ++ ")"
  (Min expr1 expr2) -> "min((" ++ (compileSExpr expr1) ++ "), (" ++ (compileSExpr expr2) ++ "))"
  (Max expr1 expr2) -> "max((" ++ (compileSExpr expr1) ++ "), (" ++ (compileSExpr expr2) ++ "))"
  (Gt expr1 expr2) -> "(" ++ (compileSExpr expr1) ++ ") > (" ++ (compileSExpr expr2) ++ ")"
  (Gte expr1 expr2) -> "(" ++ (compileSExpr expr1) ++ ") >= (" ++ (compileSExpr expr2) ++ ")"
  (Lt expr1 expr2) -> "(" ++ (compileSExpr expr1) ++ ") < (" ++ (compileSExpr expr2) ++ ")"
  (Lte expr1 expr2) -> "(" ++ (compileSExpr expr1) ++ ") <= (" ++ (compileSExpr expr2) ++ ")"
  (Shl expr1 expr2) -> "(" ++ (compileSExpr expr1) ++ ") << (" ++ (compileSExpr expr2) ++ ")"
  (Shr expr1 expr2) -> "(" ++ (compileSExpr expr1) ++ ") >> (" ++ (compileSExpr expr2) ++ ")"
  (Cast expr ty) -> "(" ++ (compileType ty) ++ ")(" ++ (compileSExpr expr) ++ ")"
  (Call name exprs) -> name ++ "(" ++ argString ++ ")" where
    exprStrings = map compileSExpr exprs
    argString = intercalate ", " exprStrings
  (FieldExpr name) -> "this." ++ name
  e -> error $ show e
