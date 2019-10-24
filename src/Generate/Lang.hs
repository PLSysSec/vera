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

if_ :: Codegen Expr
    -> Codegen [Stmt]
    -> Codegen (Maybe [Stmt])
    -> Codegen Stmt
if_ cond ifBr elseBr = error ""

-- | Assign a variable to a an expression.
-- Right now it does not support assignment to struct members, but it will have to
assign :: Codegen Leaf
       -> Codegen Expr
       -> Codegen Stmt
assign lhs' rhs' = do
  lhs <- lhs'
  rhs <- rhs'
  -- SSA the LHS
  case lhs of
    Simple name (VNode undef vnode vtype) -> do
      s0 <- get
      let versions = vers s0
      version <- case M.lookup name versions of
        Nothing  -> do
          put $ s0 { vers = M.insert name 0 versions }
          return 0
        Just ver -> do
          let newVer = ver + 1
          put $ s0 { vers = M.insert name newVer versions }
          return newVer
      newVar <- liftVerif $ newResultVar vtype $ name ++ "_" ++ show version
      error ""
    _ -> error "Cannot assign to a non-variable"

      error ""

-- Numbers and variables

number :: Type -> Integer -> Codegen Expr
number ty num = error ""

var :: Type -> String -> Codegen Expr
var = error ""

test :: Codegen Expr
test = (number Signed 5) .+. (number Signed 6) .+. (number Signed 6) .+. (number Signed 81)


