module Generate.CGen where
import Control.Monad
import Generate.SMTAST
import Generate.State
import Generate.Lang
import Data.List
import DSL.Typed       as T

compileSType :: STy -> String
compileSType stype = case stype of
  (PrimType ty) -> compileType ty
  (Class name) -> name
  void -> "void"

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
  comp <- compileSStmt s
  return $ unlines comp

compileClass :: ClassDef -> Codegen [String]
compileClass (ClassDef name fields methods) = do
  let headerStrings = ["class " ++ name ++ " {", "public:"]
      fieldStrings = map (\(argName, ty) -> (compileType ty) ++ " " ++ argName ++ ";") fields
  compFunctions <- mapM compileFunction methods
  let methodStrings = concat compFunctions
  return $ headerStrings ++ fieldStrings ++ methodStrings ++ ["};"]

compileAssignment :: String -> SExpr -> SExpr -> Codegen [String]
compileAssignment assign expr1 expr2 = do
  compExpr1 <- compileSExpr expr1
  compExpr2 <- compileSExpr expr2
  return [(unwords [compExpr1, assign, compExpr2]) ++ ";"]

compileSStmt :: SStmt -> Codegen [String]
compileSStmt stmt = case stmt of
  (Decl (SVar ty name _)) ->  return [(compileType ty) ++ " " ++ name ++ ";"]
  (Decl var) -> case var of
    SVar ty name _ -> return [(compileType ty) ++ " " ++ name ++ ";"]
    CVar cl name   -> return [cl ++ " " ++ name ++ ";"]
  (Assign expr1 expr2) -> compileAssignment "=" expr1 expr2
  (AddEq _ expr1 expr2) -> compileAssignment "+=" expr1 expr2
  (SubEq _ expr1 expr2) -> compileAssignment "-=" expr1 expr2
  (OrEq _ expr1 expr2) -> compileAssignment "|=" expr1 expr2
  (AndEq _ expr1 expr2) -> compileAssignment "&=" expr1 expr2
  Push -> return [""]
  Pop -> return [""]
  (Assert expr) -> do
    comp <- (compileSExpr expr)
    return ["assert(" ++ comp ++ ")" ++ ";"]
  (If expr thenStmts elseStmts) -> do
    compCondExpr <- (compileSExpr expr)
    let condString = "if(" ++ compCondExpr ++ ") {"
    thenStringsComp <- mapM compileSStmt thenStmts
    elseStringsComp <- mapM compileSStmt elseStmts
    let thenStrings = concat thenStringsComp
        elseStrings = concat elseStringsComp
        elseConcatString = if (null elseStrings)
          then [""]
          else ["} else {"]
    return $ [condString] ++ thenStrings ++ elseConcatString ++ elseStrings ++ ["}"]
  (Return expr) -> do
    comp <- (compileSExpr expr)
    return ["return " ++ comp ++ ";"]
  (VoidCall name args) -> do
    comp <- (compileSExpr (Call name args))
    return [comp ++ ";"]

compUnarySExpr :: String -> SExpr -> Codegen String
compUnarySExpr str expr = do
  comp <- compileSExpr expr
  return $ str ++ "(" ++ comp ++ ")"

compBinarySExpr :: String -> SExpr ->SExpr -> Codegen String
compBinarySExpr str expr1 expr2 = do
  comp1 <- compileSExpr expr1
  comp2 <- compileSExpr expr2
  return $ "(" ++ comp1 ++ ") " ++ str ++ " (" ++ comp2 ++ ")"

compileSExpr :: SExpr -> Codegen String
compileSExpr expr = case expr of
  (VarExpr var) ->
    case var of
      SVar _ name _ -> do
        fieldInfo <- getFieldInfo name
        return $ case fieldInfo of
          Just (name, fieldName) -> name ++ "->" ++ fieldName
          Nothing -> name
      CVar _ name   -> return name
  (NumExpr (SNum numType numVal)) -> return $ show numVal
  (Neg expr) -> compUnarySExpr "-" expr
  (Not expr) -> compUnarySExpr "!" expr
  (Abs expr) -> compUnarySExpr "abs" expr
  (Eq expr1 expr2) -> compBinarySExpr "==" expr1 expr2
  (And expr1 expr2) -> compBinarySExpr "&" expr1 expr2
  (Add expr1 expr2) -> compBinarySExpr "+" expr1 expr2
  (Sub expr1 expr2) -> compBinarySExpr "-" expr1 expr2
  (Mul expr1 expr2) -> compBinarySExpr "*" expr1 expr2
  (Or expr1 expr2) -> compBinarySExpr "|" expr1 expr2
  (XOr expr1 expr2) -> compBinarySExpr "^" expr1 expr2
  (Min expr1 expr2) -> do
    comp1 <- compileSExpr expr1
    comp2 <- compileSExpr expr2
    return $ "min((" ++ comp1 ++ "), (" ++ comp2 ++ "))"
  (Max expr1 expr2) -> do
    comp1 <- compileSExpr expr1
    comp2 <- compileSExpr expr2
    return $ "max((" ++ comp1 ++ "), (" ++ comp2 ++ "))"
  (Gt expr1 expr2) -> compBinarySExpr ">" expr1 expr2
  (Gte expr1 expr2) -> compBinarySExpr ">=" expr1 expr2
  (Lt expr1 expr2) -> compBinarySExpr "<" expr1 expr2
  (Lte expr1 expr2) -> compBinarySExpr "<=" expr1 expr2
  (Shl expr1 expr2) ->  compBinarySExpr "<<" expr1 expr2
  (Shr expr1 expr2) -> compBinarySExpr ">>" expr1 expr2
  (Cast expr ty) -> do
    comp <- compileSExpr expr
    return $ "(" ++ (compileType ty) ++ ")(" ++ comp ++ ")"
  (Call name exprs) -> do
    argStrings <- mapM compileSExpr exprs
    let argString = intercalate ", " argStrings
    return $ name ++ "(" ++ argString ++ ")"
  (FieldExpr name) -> return $ "this." ++ name
  e -> error $ show e
