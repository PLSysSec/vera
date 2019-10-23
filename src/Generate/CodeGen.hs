module Generate.CodeGen where
import           Control.Monad.State.Strict
import qualified DSL.Typed                  as T
import           Generate.AST
import           Generate.State

genType :: RichType
        -> Codegen String
genType rt =
  return $ case rt of
    Normal ty -> case ty of
      T.Signed8    -> "int8_t"
      T.Unsigned8  -> "uint8_t"
      T.Signed16   -> "int16_t"
      T.Unsigned16 -> "uint16_t"
      T.Signed     -> "int32_t"
      T.Unsigned   -> "uint32_t"
      T.Signed64   -> "int64_t"
      T.Unsigned64 -> "uint64_t"
    _ -> error ""

genExprCpp :: Expr
           -> Codegen String
genExprCpp expr =
  case expr of
    Add _ left right -> do
      leftCpp <- genExprCpp left
      rightCpp <- genExprCpp right
      return $ unwords $ ["(", leftCpp, "+", rightCpp, ")"]
    Sub _ left right -> do
      leftCpp <- genExprCpp left
      rightCpp <- genExprCpp right
      return $ unwords $ ["(", leftCpp, "-", rightCpp, ")"]
    Mul _ left right -> do
      leftCpp <- genExprCpp left
      rightCpp <- genExprCpp right
      return $ unwords $ ["(", leftCpp, "*", rightCpp, ")"]
    Cast ty expr -> do
      tyCpp <- genType ty
      exprCpp <- genExprCpp expr
      return $ unwords $ ["(", tyCpp, ")", "(", exprCpp, ")"]

genExprSMT :: Expr
           -> Codegen T.VNode
genExprSMT expr =
  case expr of
    Add _ left right -> do
      leftSym <- genExprSMT left
      rightSym <- genExprSMT right
      error ""
    _ -> error ""

