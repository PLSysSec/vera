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
    GetField var fieldName -> error ""
    _                      -> error "Not done"

genStmtSMT :: SStmt
           -> Codegen ()
genStmtSMT stmt =
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
        _ -> genStmtSMT stmt


