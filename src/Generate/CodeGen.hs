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
    Max{} -> error "Do not know how to generate max"
    Min{} -> error "Do not know how to generate min"
    GetField{} -> error "Do not know how to generate get field"
    Lt _ left right -> do
      leftCpp <- genExprCpp left
      rightCpp <- genExprCpp right
      return $ unwords $ [leftCpp, "<", rightCpp]
    Lte _ left right -> do
      leftCpp <- genExprCpp left
      rightCpp <- genExprCpp right
      return $ unwords $ [leftCpp, "<=", rightCpp]
    Gt _ left right -> do
      leftCpp <- genExprCpp left
      rightCpp <- genExprCpp right
      return $ unwords $ [leftCpp, ">", rightCpp]
    Gte _ left right -> do
      leftCpp <- genExprCpp left
      rightCpp <- genExprCpp right
      return $ unwords $ [leftCpp, "=>", rightCpp]
    Eq _ left right -> do
      leftCpp <- genExprCpp left
      rightCpp <- genExprCpp right
      return $ unwords $ [leftCpp, "==", rightCpp]
    And _ left right -> do
      leftCpp <- genExprCpp left
      rightCpp <- genExprCpp right
      return $ unwords $ ["(", leftCpp, "&", rightCpp, ")"]
    Or _ left right -> do
      leftCpp <- genExprCpp left
      rightCpp <- genExprCpp right
      return $ unwords $ ["(", leftCpp, "|", rightCpp, ")"]
    Xor _ left right -> do
      leftCpp <- genExprCpp left
      rightCpp <- genExprCpp right
      return $ unwords $ ["(", leftCpp, "^", rightCpp, ")"]
    LShift _ left right -> do
      leftCpp <- genExprCpp left
      rightCpp <- genExprCpp right
      return $ unwords $ ["(", leftCpp, "<<", rightCpp, ")"]
    RShift _ left right -> do
      leftCpp <- genExprCpp left
      rightCpp <- genExprCpp right
      return $ unwords $ ["(", leftCpp, ">>", rightCpp, ")"]
    Abs{} -> error "Don't know how to generate abs"
    Not _ expr -> do
      exprSym <- genExprCpp expr
      return $ unwords $ ["!", exprSym]
    Neg _ expr -> do
      exprSym <- genExprCpp expr
      return $ unwords $ ["-", exprSym]
    _ -> error "Do not support"

genExprSMT :: Expr
           -> Codegen T.VNode
genExprSMT expr =
  case expr of
    Add _ left right -> do
      leftSym <- genExprSMT left
      rightSym <- genExprSMT right
      liftVerif $ T.cppAdd leftSym rightSym
    Sub _ left right -> do
      leftSym <- genExprSMT left
      rightSym <- genExprSMT right
      liftVerif $ T.cppSub leftSym rightSym
    Mul _ left right -> do
      leftSym <- genExprSMT left
      rightSym <- genExprSMT right
      liftVerif $ T.cppMul leftSym rightSym
    Cast ty expr -> do
      exprSym <- genExprSMT expr
      liftVerif $ T.cppCast exprSym $ ntype ty
    Max _ left right -> do
      leftSym <- genExprSMT left
      rightSym <- genExprSMT right
      liftVerif $ T.cppMax leftSym rightSym
    Min _ left right -> do
      leftSym <- genExprSMT left
      rightSym <- genExprSMT right
      liftVerif $ T.cppMin leftSym rightSym
    Lt _ left right -> do
      leftSym <- genExprSMT left
      rightSym <- genExprSMT right
      liftVerif $ T.cppLt leftSym rightSym
    Lte _ left right -> do
      leftSym <- genExprSMT left
      rightSym <- genExprSMT right
      liftVerif $ T.cppLte leftSym rightSym
    Gt _ left right -> do
      leftSym <- genExprSMT left
      rightSym <- genExprSMT right
      liftVerif $ T.cppGt leftSym rightSym
    Gte _ left right -> do
      leftSym <- genExprSMT left
      rightSym <- genExprSMT right
      liftVerif $ T.cppGte leftSym rightSym
    Eq _ left right -> do
      leftSym <- genExprSMT left
      rightSym <- genExprSMT right
      liftVerif $ T.cppEq leftSym rightSym
    And _ left right -> do
      leftSym <- genExprSMT left
      rightSym <- genExprSMT right
      liftVerif $ T.cppAnd leftSym rightSym
    Or _ left right -> do
      leftSym <- genExprSMT left
      rightSym <- genExprSMT right
      liftVerif $ T.cppOr leftSym rightSym
    Xor _ left right -> do
      leftSym <- genExprSMT left
      rightSym <- genExprSMT right
      liftVerif $ T.cppXor leftSym rightSym
    LShift _ left right -> do
      leftSym <- genExprSMT left
      rightSym <- genExprSMT right
      liftVerif $ T.cppShiftLeft leftSym rightSym
    RShift _ left right -> do
      leftSym <- genExprSMT left
      rightSym <- genExprSMT right
      liftVerif $ T.cppShiftRight leftSym rightSym
    Abs _ expr -> do
      exprSym <- genExprSMT expr
      liftVerif $ T.cppAbs exprSym
    Not _ expr -> do
      exprSym <- genExprSMT expr
      liftVerif $ T.cppNot exprSym
    Neg _ expr -> do
      exprSym <- genExprSMT expr
      liftVerif $ T.cppNeg exprSym
    Simple (VV vnode _) -> return vnode
    _ -> error "Malformed leaf node"

