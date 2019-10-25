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
  e1 <- e1'
  e2 <- e2'
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
assign :: Codegen Leaf
       -> Codegen Expr
       -> Codegen Stmt
assign lhs' rhs' = do
  lhs <- lhs'
  rhs <- rhs'
  case lhs of
    V var -> do
      newVar <- nextVer var
      let newLhs = VV newVar var
      return $ Assign newLhs rhs
    _ -> error "Cannot assign to a non-variable"

--
-- Numbers and variables
--

declare :: [(Type, Variable)] -> Codegen [Stmt]
declare vars = error ""

number :: Type -> Integer -> Codegen Expr
number ty num = error ""

test :: Codegen Expr
test = (number Signed 5) .+. (number Signed 6) .+. (number Signed 6) .+. (number Signed 81)

--
-- Helpers
--

rhsVar :: Leaf -> Codegen Leaf
rhsVar (V var) = do
  node <- curVar var
  return $ VV node var
rhsVar _ = error "Cannot make rhs variable of non-variable type"

