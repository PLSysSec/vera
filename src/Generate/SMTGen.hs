module Generate.SMTGen where
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import qualified DSL.Typed                  as T
import           Generate.SMTAST
import           Generate.State

genVarSMT :: SVar
           -> Codegen T.VNode
genVarSMT var = getVar var

genExprSMT :: SExpr
           -> Codegen T.VNode
genExprSMT expr =
  case expr of
    VarExpr svar           -> genVarSMT svar
    GetField var fieldName -> getField var fieldName
    Call name args         -> do
      argSyms <- mapM genExprSMT args
      formalArgSyms <- getFormalArgs name >>= mapM getVar
      lazyBodyStmts <- getBody name
      retValSym <- getReturnVal name >>= getVar
      unless (length argSyms == length formalArgSyms) $
        error $ unwords ["Improper number of arguments to", name]
      -- Set the arguments equal to the formal arguments
      forM_ (zip argSyms formalArgSyms) $ \(a, f) -> liftVerif $ T.vassign a f
      -- Execute the function. This will re-version all the variables in the function
      -- Then, generate SMT for the function (and provide the function with the return
      -- value, so it can properly assign return statements)
      forM_ lazyBodyStmts $ \line' -> do
                              line <- line' -- Version everything
                              genStmtSMT (Just retValSym) line  -- SMT including ret val
      return retValSym
    _                      -> error "Not done"

genStmtSMT :: Maybe T.VNode -- ^ Return value, if we're generating code
                            -- for the body of a function call
           -> SStmt         -- ^ The statement to translate to SMT
           -> Codegen ()
genStmtSMT mRetVal stmt =
  case stmt of
    Decl var -> return () -- Declaration is just important for variable tracking
    Assign var expr -> do
      varSMT <- genVarSMT var
      exprSMT <- genExprSMT expr
      liftVerif $ T.vassign varSMT exprSMT
    If cond trueBr falseBr -> do
      condSym <- genExprSMT cond
      mapM_ (rewriteConditional condSym) trueBr
      notCond <- liftVerif $ T.cppNot condSym
      mapM_ (rewriteConditional notCond) falseBr
  where
    -- Guard each assignment with the given condition
    rewriteConditional :: T.VNode -> SStmt -> Codegen ()
    rewriteConditional cond stmt =
      case stmt of
        Assign var expr -> do
          curVar <- genVarSMT var
          let prevVar = SVar (varTy var) (varName var) (varVersion var - 1)
          trueBr <- genExprSMT expr
          falseBr <- genVarSMT prevVar
          conditional <- liftVerif $ T.cppCond cond trueBr falseBr
          liftVerif $ T.vassign curVar conditional
        _ -> genStmtSMT mRetVal stmt


