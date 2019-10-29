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

      retValSym <- getReturnVal name >>= getVar

      lazyBodyStmts <- getBody name
      unless (length argSyms == length formalArgSyms) $
        error $ unwords ["Improper number of arguments to", name]
      -- Set the arguments equal to the formal arguments
      forM_ (zip (concat argSyms) (concat formalArgSyms)) $
        \(a, f) -> liftVerif $ T.vassign a f

      -- Execute the function. This will re-version all the variables in the function
      -- Then, generate SMT for the function (and provide the function with the return
      -- value, so it can properly assign return statements)
      forM_ lazyBodyStmts $ \line' -> do
                              line <- line' -- Re-version everything
                              genStmtSMT (Just retValSym) line  -- SMT including ret val

      return retValSym
    _                      -> error "Not done"
  where

genStmtSMT :: Maybe T.VNode -- ^ Return value, if we're generating code
                            -- for the body of a function call
           -> SStmt         -- ^ The statement to translate to SMT
           -> Codegen ()
genStmtSMT mRetVal stmt =
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
    Return expr -> case mRetVal of
      Nothing -> return ()
      Just rval -> do
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
        Return expr ->
          case mRetVal of
            -- We aren't in a function call, so we don't need to do anything
            Nothing -> return ()
            Just rval -> do
              exprSym <- genExprSMT expr
              conditionalFalse <- liftVerif $ T.cppNot cond
              rvalIsExpr <- liftVerif $ T.cppEq rval exprSym
              liftVerif $ T.cppOr conditionalFalse rvalIsExpr >>= T.vassert
        _ -> genStmtSMT mRetVal stmt

genBodySMT :: [Codegen SStmt] -> Codegen ()
genBodySMT body = forM_ body $ \line -> line >>= \l -> genStmtSMT Nothing l

listOf :: a -> [a]
listOf x = [x]
