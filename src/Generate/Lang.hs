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

define_ :: FunctionName
        -> STy
        -> [(VarName, STy)]
        -> [Codegen SStmt]
        -> Codegen ()
define_ funName funTy funArgs body = do
  -- Declare all the argument variables and the return value
  forM_ funArgs $ \(name, ty) -> newVar ty name
  let retValName = funName ++ "_return_val"
  newVar funTy retValName
  -- Save the relevant information in the state so we can call it later
  addFunction funName (map fst funArgs) retValName body

class_ :: ClassName -> [(FieldName, Type)] -> Codegen ClassDef
class_ name fields = do
  let fieldMap = M.fromList fields
  addClass name fieldMap
  return $ ClassDef name fieldMap

--
-- Variables and numbers
--

v :: VarName -> Codegen SVar
v name = do
  ty <- varType name
  ver <- curVersion name
  return $ SVar ty name ver

ve :: VarName -> Codegen SExpr
ve name = v name >>= return . VarExpr

n :: Type -> Int -> Codegen SNum
n ty num = return $ SNum ty num

ne :: Type -> Int -> Codegen SExpr
ne ty num = n ty num >>= return . NumExpr

--
-- Operators
--

(.->.) :: SVar -> FieldName -> Codegen SExpr
(.->.) var fieldname = do
  when (isPrimType var) $
    error $ unwords ["Cannot get field of primitived typed", varName var]
  return $ GetField var fieldname

--
-- Statements
--

declare :: STy -> VarName -> Codegen SStmt
declare ty var = do
  newVar ty var
  return $ Decl $ SVar ty var 0
declare _ _ = error "Class type is not set up yet"

assign :: Codegen SVar
       -> Codegen SExpr
       -> Codegen SStmt
assign svar' sexpr' = do
  svar <- svar'
  sexpr <- sexpr'
  unless (isPrimType svar) $ error "Cannot assign to struct field"
  let toReturn = Assign svar sexpr
  nextVersion (varName svar)
  return toReturn

if_ :: Codegen SExpr
    -> [Codegen SStmt]
    -> [Codegen SStmt]
    -> Codegen SStmt
if_ cond' ifBr' elseBr' = do
  cond <- cond'
  ifBr <- forM ifBr' $ \line -> line
  elseBr <- forM elseBr' $ \line -> line
  return $ If cond ifBr elseBr





-- binOp :: Codegen Expr
--       -> Codegen Expr
--       -> (RichType -> Expr -> Expr -> Expr)
--       -> String
--       -> Codegen Expr
-- binOp e1' e2' constructor opName = do
--   e1 <- e1' >>= normExpr
--   e2 <- e2' >>= normExpr
--   return $ constructor (Normal Signed) e1 e2

-- (.+.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
-- (.+.) e1 e2 = binOp e1 e2 Add "add"

-- (.-.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
-- (.-.) e1 e2 = binOp e1 e2 Sub "sub"

-- (.*.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
-- (.*.) e1 e2 = binOp e1 e2 Mul "mul"

-- (.<.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
-- (.<.) e1 e2 = binOp e1 e2 Lt "lt"

-- (.<=.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
-- (.<=.) e1 e2 = binOp e1 e2 Lte "lte"

-- (.>.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
-- (.>.) e1 e2 = binOp e1 e2 Gt "gt"

-- (.=>.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
-- (.=>.) e1 e2 = binOp e1 e2 Gt "gte"

-- (.==.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
-- (.==.) e1 e2 = binOp e1 e2 Eq "eq"

-- (.&.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
-- (.&.) e1 e2 = binOp e1 e2 And "and"

-- (.|.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
-- (.|.) e1 e2 = binOp e1 e2 Or "or"

-- (.^.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
-- (.^.) e1 e2 = binOp e1 e2 Xor "xor"

-- (.<<.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
-- (.<<.) e1 e2 = binOp e1 e2 LShift "lshift"

-- (.>>.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
-- (.>>.) e1 e2 = binOp e1 e2 RShift "rshift"

-- (.->.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
-- (.->.) e1' e2' = do
--   e1 <- e1'
--   let name = case e1 of
--                Simple (ClassObj cname) -> cname
--                _                       -> error "No nested classes"
--   e2 <- e2'
--   let fieldName = case e2 of
--                     Simple (Field name) -> name
--                     _                   -> error "No nested field names"
--   return $ GetField (Class name) name fieldName

-- call :: String
--      -> [Codegen Expr]
--      -> Codegen Expr
-- call name args' = do
--   ty <- getFunctionType name
--   args <- forM args' $ \arg -> arg
--   return $ Call name ty args

-- -- Statements

-- -- |
-- if_ :: Codegen Expr -- ^ Condition
--     -> [Codegen Stmt] -- ^ True branch
--     -> Maybe ([Codegen Stmt]) -- ^ Possible false branch
--     -> Codegen Stmt -- ^ Resulting if statement
-- if_ cond' ifBr' elseBr' = do
--   cond <- cond'
--   ifBr <- forM ifBr' $ \line -> line
--   elseBr <- if isJust elseBr'
--             then forM (fromJust elseBr') $ \line -> line
--             else return []
--   return $ If cond ifBr elseBr

-- -- | Assign a variable to a an expression.
-- -- Right now it does not support assignment to struct members, but it will have to
-- assign :: Codegen Expr
--        -> Codegen Expr
--        -> Codegen Stmt
-- assign lhs' rhs' = do
--   lhs <- lhs'
--   rhs <- rhs'
--   case lhs of
--     Simple (V var) -> do
--       (newVar, newVer) <- nextVer var
--       let newLhs = Simple $ VV newVar var newVer
--       return $ Assign newLhs rhs
--     _ -> error "Cannot assign to a class field or other non-simple variable"

-- returnFrom :: String
--            -> Codegen Expr
--            -> Codegen Stmt
-- returnFrom str expr' = do
--   expr <- expr' >>= normExpr
--   return $ Return str expr

-- --
-- -- Functions, programs, and class definitions
-- --

-- class_ :: String
--        -> [(String, Type)]
--        -> Codegen ClassDef
-- class_ name fields = do
--   addClass name fields
--   let fs = map (\(f, t) -> FieldDef t f)  fields
--   return $ ClassDef fs

-- define :: String
--        -> Type
--        -> [(Variable, Type)]
--        -> [Codegen Stmt]
--        -> Codegen Function
-- define fnName returnType args body' = do
--   forM_ args $ \(var, ty) -> do
--     -- Declare the arguments, then make a new var so that they exist
--     void $ declare ty var
--     void $ nextVer var
--   addFunction fnName body' returnType args
--   body <- forM body' $ \line -> line
--   return $ Function fnName returnType args body

-- --
-- -- Numbers and variables
-- --

-- declare :: Type -> Variable -> Codegen Stmt
-- declare ty var = do
--   void $ newVar ty var
--   return $ Decl var ty

-- number :: Type -> Integer -> Codegen Expr
-- number ty n = do
--   node <- case ty of
--             Signed -> liftVerif $ num n
--             _      -> error ""
--   return $ Simple $ N node

-- v :: Variable -> Codegen Expr
-- v var = return $ Simple $ V var

-- test :: [Codegen Stmt]
-- test = [ declare Signed "lhs"
--        , declare Signed "var"
--        , (v "lhs") `assign` ((number Signed 5) .+. (v "var"))
--        ]

-- test2 :: Codegen ()
-- test2 = do
--   forM_ test $ \line -> do
--     result <- line
--     return ()

-- --
-- -- Helpers
-- --

-- normExpr :: Expr -> Codegen Expr
-- normExpr (Simple leaf) = rhsVar leaf >>= return . Simple
-- normExpr e             = return e

-- rhsVar :: Leaf -> Codegen Leaf
-- rhsVar (V var) = do
--   (node, ver) <- curVar var
--   return $ VV node var ver
-- rhsVar n@N{} = return n
-- rhsVar _ = error "Cannot make rhs variable of non-variable type"
