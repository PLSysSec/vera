{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Generate.State where
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           DSL.DSL                    hiding (vars)
import           DSL.Typed
import qualified Z3.Monad                   as Z

type Variable = String

data CodegenState = CodegenState { clases :: Int
                                 , vars   :: M.Map String [VNode]
                                 }

-- | Make (and return) a new variable
newVar :: Type -> Variable -> Codegen VNode
newVar ty var = do
  s0 <- get
  let allVars = vars s0
  when (M.member var allVars) $ error $ unwords ["Variable", var, "already declared"]
  newVar <- liftVerif $ newResultVar ty $ var ++ "_0"
  put $ s0 { vars = M.insert var [newVar] allVars }
  return newVar

-- | Get the most recent version of a variable
curVar :: Variable -> Codegen VNode
curVar var = varVer var 0

-- | Get the ver version of a variable
varVer :: Variable -> Int -> Codegen VNode
varVer var ver = do
  allVars <- vars `liftM` get
  case M.lookup var allVars of
    Nothing -> error $ unwords [var, "has not been declared"]
    Just vs -> if length vs > ver
               then return $ vs !! ver
               else error $ unwords [var, "has no version", show ver]

-- | Get an increased version of the variable
nextVer :: Variable -> Codegen VNode
nextVer var = do
  s0 <- get
  let allVars = vars s0
  case M.lookup var allVars of
    Nothing -> error $ unwords [var, "has not been declared"]
    Just vs -> if null vs
               then error $ unwords ["Malformed version info for", var]
               else do
                 let ty = vtype $ head vs
                     ver = length vs
                 newVar <- liftVerif $ newResultVar ty $ var ++ "_" ++ show ver
                 put $ s0 { vars = M.insert var (newVar:vs) allVars }
                 return newVar

newtype Codegen a = Codegen (StateT CodegenState Verif a)
    deriving (Functor, Applicative, Monad, MonadState CodegenState, MonadIO)

liftVerif :: Verif a -> Codegen a
liftVerif = Codegen . lift

instance Z.MonadZ3 Codegen where
    getSolver = Codegen $ lift $ Z.getSolver
    getContext = Codegen $ lift $ Z.getContext
