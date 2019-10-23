module Generate.Lang where
import           Control.Monad  (unless)
import qualified DSL.DSL        as D
import           DSL.Typed
import           Generate.AST
import           Generate.State
import           Prelude        hiding ((+))

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

(+) :: Codegen Expr -> Codegen Expr -> Codegen Expr
(+) e1 e2 = binOp e1 e2 Add "add"

(-) :: Codegen Expr -> Codegen Expr -> Codegen Expr
(-) e1 e2 = binOp e1 e2 Sub "sub"

(*) :: Codegen Expr -> Codegen Expr -> Codegen Expr
(*) e1 e2 = binOp e1 e2 Mul "mul"

-- ifstmt :: Codegen Expr
--        -> Codegen [Stmt]
--        -> Codegen (Maybe [Stmt])
-- ifstmt cond ifBr elseBr = error ""

number :: Type -> Integer -> Codegen Expr
number ty num = error ""

var :: Type -> String -> Codegen Expr
var = error ""

test :: Codegen Expr
test = (number Signed 5) + (number Signed 6) + (number Signed 6) + (number Signed 81)


