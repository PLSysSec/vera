module Generate.Lang where
import           Control.Monad              (unless)
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           Data.Maybe                 (catMaybes, fromJust, isJust)
import qualified DSL.DSL                    as D
import           DSL.Typed
import           Generate.SMTAST
import           Generate.State

--
-- Top-level declarations and definitions
--

data FunctionDef = Function { fName :: FunctionName
                            , fTy   :: STy
                            , fArgs :: [(VarName, STy)]
                            , fBody :: [Codegen SStmt]
                            }

data ClassDef = ClassDef ClassName [(FieldName, Type)] [FunctionDef]

define :: FunctionDef
       -> Codegen ()
define f = define' f Nothing

define' :: FunctionDef
        -> Maybe SVar
        -> Codegen ()
define' (Function funName funTy funArgs body) svar = do
  -- Declare all the argument variables and the return value
  forM_ funArgs $ \(name, ty) -> newVar ty name
  let retValName = funName ++ "_return_val"
  newVar funTy retValName
  -- Save the relevant information in the state so we can call it later
  addFunction funName (map fst funArgs) retValName body svar

class_ :: ClassDef -> Codegen ()
class_ (ClassDef name fields functions) = do
  liftIO $ putStrLn $ unwords ["Defining class:", name]
  addClass name $ M.fromList fields
  forM_ functions $ \(Function funName funTy funArgs body) -> do
    let newName = name ++ "_" ++ funName
    define' (Function newName funTy funArgs body) Nothing

--
-- Variables and numbers
--

-- | Make a primitive type
t :: Type -> STy
t = PrimType

-- | Make a class type
c :: String -> STy
c = Class

-- | Get a declared variable
v :: VarName -> Codegen SExpr
v name = do
  ty <- varType name
  if isClass ty
  then return $ VarExpr $ CVar (className ty) name
  else curVar name >>= return . VarExpr

-- | Get a number
n :: Type -> Integer -> Codegen SExpr
n ty num = return $ NumExpr $ SNum ty num

-- | Get field from field name in a class method
f :: FieldName -> Codegen SExpr
f name = return $ FieldExpr name

--
-- Operators
--

call :: String
     -> [Codegen SExpr]
     -> Codegen SExpr
call name args' = do
  args <- forM args' $ \arg -> arg
  return $ Call name args

(.+.) :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
(.+.) left' right' = do
  left <- left'
  right <- right'
  return $ Add left right

(.<.) :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
(.<.) left' right' = do
  left <- left'
  right <- right'
  return $ Lt left right

(.->.) :: Codegen SExpr -> FieldName -> Codegen SExpr
(.->.) ve' fieldname = do
  ve <- ve'
  unless (isClassExpr ve) $ error $ unwords ["Cannot get field of non class", show ve]
  getField (exprVar ve) fieldname >>= return . VarExpr

method :: Codegen SExpr -> FunctionName -> [Codegen SExpr] -> Codegen SExpr
method ve' funName args' = do
  ve <- ve'
  args <- forM args' $ \arg -> arg
  unless (isClassExpr ve) $ error $ unwords ["Cannot get method of non class", show ve]
  let fullFunName = (varClass $ exprVar ve) ++ "_" ++ funName
  return $ Call fullFunName (ve:args)

--
-- Statements
--

declare :: STy -> VarName -> Codegen SStmt
declare ty var = do
  newVar ty var
  return $ if isClass ty
  then Decl $ CVar (className ty) var
  else Decl $ SVar (primTy ty) var 0
declare _ _ = error "Class type is not set up yet"

assign :: Codegen SExpr
       -> Codegen SExpr
       -> Codegen SStmt
assign svar' sexpr' = do
  svar <- svar'
  sexpr <- sexpr'
  unless (isClassExpr svar || isPrimVarExpr svar) $ error "Cannot assign to non-variable"
  when (isClassExpr svar) $ unless (isClassExpr sexpr) $
    error "Cannot assign class to non-class"
  newVar <- nextVar (varName $ exprVar svar)
  return $ Assign (VarExpr newVar) sexpr

if_ :: Codegen SExpr
    -> [Codegen SStmt]
    -> [Codegen SStmt]
    -> Codegen SStmt
if_ cond' ifBr' elseBr' = do
  cond <- cond'
  ifBr <- forM ifBr' $ \line -> line
  elseBr <- forM elseBr' $ \line -> line
  return $ If cond ifBr elseBr

return_ :: Codegen SExpr
        -> Codegen SStmt
return_ expr' = do
  expr <- expr'
  return $ Return expr
