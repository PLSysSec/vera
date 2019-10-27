module Generate.SMTGen where
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import qualified DSL.Typed                  as T
import           Generate.SMTAST
import           Generate.State

genSVarSMT :: SVar
           -> Codegen T.VNode
genSVarSMT var = getVar var

genExprSMT :: SExpr
           -> Codegen T.VNode
genExprSMT expr =
  case expr of
    VarExpr svar -> genSVarSMT svar
    _            -> error "Not done"
