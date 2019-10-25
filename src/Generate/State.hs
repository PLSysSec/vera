{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Generate.State where
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           DSL.DSL                    hiding (vars)
import           DSL.Typed
import           Generate.AST
import qualified Z3.Monad                   as Z

data CodegenState = CodegenState { vars :: M.Map String [VNode]
                                 , tys  :: M.Map String Type
                                 , funs :: M.Map String Function
                                 }


emptyCodegenState :: CodegenState
emptyCodegenState = CodegenState M.empty M.empty M.empty

addFunction :: String
            -> Function
            -> Codegen ()
addFunction str func = do
  s0 <- get
  let allFuns = funs s0
  case M.lookup str allFuns of
    Just _  -> error $ unwords $ ["Already defined", str]
    Nothing -> put $ s0 { funs = M.insert str func allFuns}

-- | Make (and return) a new variable
newVar :: Type -> Variable -> Codegen ()
newVar ty var = do
  s0 <- get
  let allVars = vars s0
  when (M.member var allVars) $ error $ unwords ["Variable", var, "already declared"]
  put $ s0 { vars = M.insert var [] allVars
           , tys = M.insert var ty $ tys s0
           }

-- | Get the most recent version of a variable
curVar :: Variable -> Codegen (VNode, Version)
curVar var = do
  allVars <- vars `liftM` get
  case M.lookup var allVars of
    Nothing -> error $ unwords [var, "has not been declared"]
    Just vs -> do
      if null vs
      then error $ unwords [var, "has no versions at all"]
      else return $ (head vs, length vs - 1)

-- | Get the ver version of a variable
varVer :: Variable -> Int -> Codegen VNode
varVer var ver = do
  allVars <- vars `liftM` get
  case M.lookup var allVars of
    Nothing -> error $ unwords [var, "has not been declared"]
    Just vs -> if length vs > ver
               then return $ (reverse vs) !! ver
               else error $ unwords [var, "has no version", show ver]

-- | Get an increased version of the variable
nextVer :: Variable -> Codegen (VNode, Version)
nextVer var = do
  s0 <- get
  let allVars = vars s0
  case M.lookup var allVars of
    Nothing -> error $ unwords [var, "has not been declared"]
    Just vs -> if null vs
               then do
                 let ty = tys s0 M.! var
                 newVar <- liftVerif $ newResultVar ty $ var ++ "_0"
                 put $ s0 { vars = M.insert var (newVar:vs) allVars }
                 return (newVar, 0)
               else do
                 let ty = vtype $ head vs
                     ver = length vs
                 newVar <- liftVerif $ newResultVar ty $ var ++ "_" ++ show ver
                 put $ s0 { vars = M.insert var (newVar:vs) allVars }
                 return (newVar, ver)

newtype Codegen a = Codegen (StateT CodegenState Verif a)
    deriving (Functor, Applicative, Monad, MonadState CodegenState, MonadIO)

liftVerif :: Verif a -> Codegen a
liftVerif = Codegen . lift

instance Z.MonadZ3 Codegen where
    getSolver = Codegen $ lift $ Z.getSolver
    getContext = Codegen $ lift $ Z.getContext

runCodegen :: Maybe Integer -- ^ Optional timeout
           -> Codegen a       -- ^ Codegen computation
           -> IO (a, CodegenState)
runCodegen mTimeout (Codegen act) = evalVerif mTimeout $ runStateT act emptyCodegenState

evalCodegen :: Maybe Integer -> Codegen a -> IO a
evalCodegen mt act = fst <$> runCodegen mt act

execCodegen :: Maybe Integer -> Codegen a -> IO CodegenState
execCodegen mt act = snd <$> runCodegen mt act

runSolverOnSMT :: Codegen SMTResult
runSolverOnSMT = liftVerif runSolver
