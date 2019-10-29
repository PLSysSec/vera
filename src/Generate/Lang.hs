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

define :: FunctionName
       -> STy
       -> [(VarName, STy)]
       -> [Codegen SStmt]
       -> Codegen ()
define funName funTy funArgs body = do
  -- Declare all the argument variables and the return value
  forM_ funArgs $ \(name, ty) -> newVar ty name
  let retValName = funName ++ "_return_val"
  newVar funTy retValName
  -- Save the relevant information in the state so we can call it later
  addFunction funName (map fst funArgs) retValName body

class_ :: ClassName -> [(FieldName, Type)] -> Codegen ()
class_ name fields = addClass name $ M.fromList fields

--
-- Variables and numbers
--

t :: Type -> STy
t = PrimType

c :: String -> STy
c = Class

v :: VarName -> Codegen SVar
v name = do
  ty <- varType name
  if isClass ty
  then return $ CVar (className ty) name
  else curVar name

ve :: VarName -> Codegen SExpr
ve name = v name >>= return . VarExpr

n :: Type -> Integer -> Codegen SNum
n ty num = return $ SNum ty num

ne :: Type -> Integer -> Codegen SExpr
ne ty num = n ty num >>= return . NumExpr

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

(.->.) :: Codegen SVar -> FieldName -> Codegen SExpr
(.->.) var' fieldname = do
  var <- var'
  when (isPrimType var) $ do
    liftIO $ print var
    error $ unwords ["Cannot get field of primitived typed", varName var]
  let name = varName var ++ "_" ++ fieldname
  curVar name >>= return . VarExpr

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

assign :: Codegen SVar
       -> Codegen SExpr
       -> Codegen SStmt
assign svar' sexpr' = do
  svar <- svar'
  sexpr <- sexpr'
  unless (isPrimType svar) $ error "Cannot assign to struct field"
  newVar <- nextVar (varName svar)
  return $ Assign newVar sexpr

fieldAssign :: Codegen SExpr
            -> Codegen SExpr
            -> Codegen SStmt
fieldAssign left' right' = do
  left <- left'
  right <- right'
  case left of
    VarExpr svar -> do
      newVar <- nextVar (varName svar)
      return $ Assign newVar right
    _              -> error "Malformed field assignment"


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
