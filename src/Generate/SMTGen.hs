module Generate.SMTGen where
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import qualified DSL.Typed                  as T
import           Generate.SMTAST
import           Generate.State

genVarSMT :: SVar
           -> Codegen T.VNode
genVarSMT var = getVar var

genNumSMT :: SNum
          -> Codegen T.VNode
genNumSMT snum = do
  let val = numVal snum
  liftVerif $ case numTy snum of
    T.Signed16   -> T.num16 val
    T.Unsigned16 -> T.unum16 val
    T.Signed     -> T.num val
    T.Unsigned   -> T.unum val
    T.Signed64   -> T.num64 val
    T.Unsigned64 -> T.unum64 val
    _            -> error "Float not supported"

genExprSMT :: SExpr
           -> Codegen T.VNode
genExprSMT expr =
  case expr of
    VarExpr svar           -> genVarSMT svar
    NumExpr snum           -> genNumSMT snum
    FieldExpr fieldName    -> do
      cv <- getClassVar
      case cv of
        Nothing  -> error "Cannot do plain field access outside of class method"
        Just var -> getField var fieldName >>= getVar
    Add left right         -> do
      leftSym <- genExprSMT left
      rightSym <- genExprSMT right
      liftVerif $ T.cppAdd leftSym rightSym
    Lt left right          -> do
      leftSym <- genExprSMT left
      rightSym <- genExprSMT right
      liftVerif $ T.cppLt leftSym rightSym
    Call name args         -> do

      argSyms <- forM args $ \arg -> do
        if isClassExpr arg
        then getFieldVars (exprVar arg) >>= mapM genVarSMT
        else genExprSMT arg >>= return . listOf

      formalArgSyms <- do
        fas <- getFormalArgs name
        forM fas $ \fa -> if isPrimType fa
                          then genVarSMT fa >>= return . listOf
                          else getFieldVars fa >>= mapM genVarSMT

      unless (length argSyms == length formalArgSyms) $
        error $ unwords ["Improper number of arguments to", name]
      -- Set the arguments equal to the formal arguments
      forM_ (zip (concat argSyms) (concat formalArgSyms)) $
        \(a, f) -> liftVerif $ T.vassign a f

      lazyBodyStmts <- getBody name

      let newClassVar = exprVar $ head args
      setClassVar newClassVar

      retValSym <- getReturnVal name >>= getVar
      setReturnValue [retValSym]

      -- Execute the function. This will re-version all the variables in the function
      -- Then, generate SMT for the function (and provide the function with the return
      -- value, so it can properly assign return statements)
      forM_ lazyBodyStmts $ \line' -> do
                              line <- line'    -- Re-version everything
                              genStmtSMT line  -- SMT the line. The line has access to the
                                               -- the function's return value and possible
                                               -- class var via the underlying monad

      clearClassVar
      clearRetVal
      return retValSym
    _                      -> error "Not done"
  where

genStmtSMT :: SStmt -> Codegen ()
genStmtSMT stmt =
  case stmt of
    Decl var -> return () -- Declaration is just important for variable tracking
    Assign var expr -> do
      varSMT <- genExprSMT var
      exprSMT <- genExprSMT expr
      liftVerif $ T.vassign varSMT exprSMT
    If cond trueBr falseBr -> do
      condSym <- genExprSMT cond
      mapM_ (rewriteConditional condSym) trueBr
      notCond <- liftVerif $ T.cppNot condSym
      mapM_ (rewriteConditional notCond) falseBr
    Return expr -> do
      rv <- getReturnValue
      case rv of
        [] -> return ()
        [rval] -> do
          exprSym <- genExprSMT expr
          liftVerif $ T.vassign rval exprSym
  where
    rewriteAssign :: T.VNode -> SVar -> SExpr -> Codegen ()
    rewriteAssign cond var expr = do
      when (varVersion var <= 1) $
        error $ unwords $ ["Cannot initially define var in if-stmt:", varName var]
      curVar <- genVarSMT var
      let prevVar = SVar (varTy var) (varName var) (varVersion var - 1)
      trueBr <- genExprSMT expr
      falseBr <- genVarSMT prevVar
      conditional <- liftVerif $ T.cppCond cond trueBr falseBr
      liftVerif $ T.vassign curVar conditional
    -- Guard each assignment with the given condition
    rewriteConditional :: T.VNode -> SStmt -> Codegen ()
    rewriteConditional cond stmt =
      case stmt of
        Assign (VarExpr var) expr ->
          if isPrimType var
          then rewriteAssign cond var expr
          else do
            fieldVars <- getFieldVars var
            forM_ fieldVars $ \var -> rewriteAssign cond var expr
        Return expr -> do
          rv <- getReturnValue
          case rv of
            [] -> return ()
            [rval] -> do
              exprSym <- genExprSMT expr
              conditionalFalse <- liftVerif $ T.cppNot cond
              rvalIsExpr <- liftVerif $ T.cppEq rval exprSym
              liftVerif $ T.cppOr conditionalFalse rvalIsExpr >>= T.vassert
        _ -> genStmtSMT stmt

genBodySMT :: [Codegen SStmt] -> Codegen ()
genBodySMT body = forM_ body $ \line -> line >>= genStmtSMT

listOf :: a -> [a]
listOf x = [x]
