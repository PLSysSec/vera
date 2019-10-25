module Generate.Lang where
import           Control.Monad              (unless)
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
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
  unless (ty e1 == ty e2) $ error $ unwords ["Typecheck failure:", opName]
  return $ constructor (ty e1) e1 e2

(.+.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
(.+.) e1 e2 = binOp e1 e2 Add "add"

(.-.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
(.-.) e1 e2 = binOp e1 e2 Sub "sub"

(.*.) :: Codegen Expr -> Codegen Expr -> Codegen Expr
(.*.) e1 e2 = binOp e1 e2 Mul "mul"

-- Statements

-- |
if_ :: Codegen Expr -- ^ Condition
    -> Codegen [Stmt] -- ^ True branch
    -> Codegen (Maybe [Stmt]) -- ^ Possible false branch
    -> Codegen Stmt -- ^ Resulting if statement
if_ cond ifBr elseBr = error ""


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
      newVar <- nextVer var
      let newLhs = Simple $ VV newVar var
      return $ Assign newLhs rhs
    _ -> error "Cannot assign to a non-variable"

--
-- Numbers and variables
--

declare :: Type -> Variable -> Codegen Stmt
declare ty var = do
  void $ newVar ty var
  return $ Decl var ty

number :: Type -> Integer -> Codegen Expr
number ty num = error ""

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
  node <- curVar var
  return $ VV node var
rhsVar _ = error "Cannot make rhs variable of non-variable type"

