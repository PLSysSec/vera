module Generate.Lang where
import           Control.Monad              (unless)
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           Data.Maybe                 (catMaybes, fromJust, isJust)
import qualified DSL.DSL                    as D
import           DSL.Typed
import           Generate.AST
import           Generate.State

binOp :: Codegen Expr
      -> Codegen Expr
      -> (RichType -> Expr -> Expr -> Expr)
      -> String
      -> Codegen Expr
binOp e1' e2' constructor opName = do
  e1 <- e1' >>= normExpr
  e2 <- e2' >>= normExpr
  return $ constructor (Normal Signed) e1 e2

(.+.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
(.+.) e1 e2 = binOp e1 e2 Add "add"

(.-.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
(.-.) e1 e2 = binOp e1 e2 Sub "sub"

(.*.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
(.*.) e1 e2 = binOp e1 e2 Mul "mul"

(.<.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
(.<.) e1 e2 = binOp e1 e2 Lt "lt"

(.<=.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
(.<=.) e1 e2 = binOp e1 e2 Lte "lte"

(.>.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
(.>.) e1 e2 = binOp e1 e2 Gt "gt"

(.=>.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
(.=>.) e1 e2 = binOp e1 e2 Gt "gte"

(.==.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
(.==.) e1 e2 = binOp e1 e2 Eq "eq"

(.&.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
(.&.) e1 e2 = binOp e1 e2 And "and"

(.|.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
(.|.) e1 e2 = binOp e1 e2 Or "or"

(.^.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
(.^.) e1 e2 = binOp e1 e2 Xor "xor"

(.<<.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
(.<<.) e1 e2 = binOp e1 e2 LShift "lshift"

(.>>.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
(.>>.) e1 e2 = binOp e1 e2 RShift "rshift"

call :: String
     -> [Codegen Expr]
     -> Codegen Expr
call name args' = do
  ty <- getFunctionType name
  args <- forM args' $ \arg -> arg
  return $ Call name ty args

-- Statements

-- |
if_ :: Codegen Expr -- ^ Condition
    -> [Codegen Stmt] -- ^ True branch
    -> Maybe ([Codegen Stmt]) -- ^ Possible false branch
    -> Codegen Stmt -- ^ Resulting if statement
if_ cond' ifBr' elseBr' = do
  cond <- cond'
  ifBr <- forM ifBr' $ \line -> line
  elseBr <- if isJust elseBr'
            then forM (fromJust elseBr') $ \line -> line
            else return []
  return $ If cond ifBr elseBr

-- | Assign a variable to a an expression.
-- Right now it does not support assignment to struct members, but it will have to
assign :: Codegen Expr
       -> Codegen Expr
       -> Codegen Stmt
assign lhs' rhs' = do
  lhs <- lhs'
  rhs <- rhs'
  case lhs of
    Simple (V var) -> do
      (newVar, newVer) <- nextVer var
      let newLhs = Simple $ VV newVar var newVer
      return $ Assign newLhs rhs
    _ -> error "Cannot assign to a non-variable"

--
-- Functions
--

define :: String
       -> Type
       -> [(Variable, Type)]
       -> Codegen [Stmt]
       -> Codegen Function
define fnName returnType args body' = do
  argSyms <- forM args $ \(var, ty) -> do
     void $ declare ty var
     v var >>= normExpr >>= return . verboseNode . leaf
  body <- body'
  let func = Function fnName returnType argSyms  body
  addFunction fnName func
  return func

--
-- Numbers and variables
--

declare :: Type -> Variable -> Codegen Stmt
declare ty var = do
  void $ newVar ty var
  return $ Decl var ty

number :: Type -> Integer -> Codegen Expr
number ty n = do
  node <- case ty of
            Signed -> liftVerif $ num n
            _      -> error ""
  return $ Simple $ N node

v :: Variable -> Codegen Expr
v var = return $ Simple $ V var

test :: [Codegen Stmt]
test = [ declare Signed "lhs"
       , declare Signed "var"
       , (v "lhs") `assign` ((number Signed 5) .+. (v "var"))
       ]

test2 :: Codegen ()
test2 = do
  forM_ test $ \line -> do
    result <- line
    return ()

--
-- Helpers
--

normExpr :: Expr -> Codegen Expr
normExpr (Simple leaf) = rhsVar leaf >>= return . Simple
normExpr e             = return e

rhsVar :: Leaf -> Codegen Leaf
rhsVar (V var) = do
  (node, ver) <- curVar var
  return $ VV node var ver
rhsVar n@N{} = return n
rhsVar _ = error "Cannot make rhs variable of non-variable type"
