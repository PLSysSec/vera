{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Generate.State where
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           DSL.DSL                    hiding (vars)
import           DSL.Typed
import           Generate.SMTAST
import qualified Z3.Monad                   as Z

data CodegenState = CodegenState { vars :: M.Map VarName Version
                                 , tys  :: M.Map VarName STy
                                 }


newtype Codegen a = Codegen (StateT CodegenState Verif a)
    deriving (Functor, Applicative, Monad, MonadState CodegenState, MonadIO)

instance Z.MonadZ3 Codegen where
    getSolver = Codegen $ lift $ Z.getSolver
    getContext = Codegen $ lift $ Z.getContext

emptyCodegenState :: CodegenState
emptyCodegenState = CodegenState M.empty M.empty

liftVerif :: Verif a -> Codegen a
liftVerif = Codegen . lift

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

--
-- Making new variables and versions
--

newVar :: STy -> String -> Codegen ()
newVar ty str = do
  s0 <- get
  let allVars = vars s0
      allTys = tys s0
  put $ s0 { vars = M.insert str 0 allVars
           , tys = M.insert str ty allTys
           }

-- data CodegenState = CodegenState { vars        :: M.Map String [VNode]
--                                  , tys         :: M.Map String Type
--                                  , funBodies   :: M.Map String [Codegen Stmt]
--                                  , funTypes    :: M.Map String Type
--                                  , funRetVals  :: M.Map String [VNode]
--                                  , funArgTypes :: M.Map String [(String, Type)]
--                                  , classFields :: M.Map String (M.Map String Type)
--                                  , classVars   :: M.Map String [M.Map String VNode]
--                                  }


-- emptyCodegenState :: CodegenState
-- emptyCodegenState = CodegenState M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty

-- addClass :: String
--          -> [(String, Type)]
--          -> Codegen ()
-- addClass className fields = do
--   s0 <- get
--   put $ s0 { classFields = M.insert className (M.fromList fields) $ classFields s0 }

-- -- newClassVar :: String
-- --             -> String
-- --             -> Codegen ()
-- -- newClassVar varName className = do
-- --   s0 <- get
-- --   case M.lookup className $ classFields s0 of
-- --     Nothing -> error ["Type", className, "undefined"]
-- --     Just fields -> case M.lookup varName classVars of

-- getField :: String
--          -> String
--          -> Codegen VNode
-- getField varName fieldName = do
--   s0 <- get
--   case M.lookup varName $ classVars s0 of
--     Nothing -> error $ unwords ["Variable", varName, "undeclared"]
--     Just cv -> case M.lookup fieldName $ head cv of
--                  Nothing -> error $ unwords ["Variable", varName, "has no field", fieldName]
--                  Just field -> return field

-- addFunction :: String
--             -> [Codegen Stmt]
--             -> Type
--             -> [(String, Type)]
--             -> Codegen ()
-- addFunction name body retType argTypes = do
--   s0 <- get
--   let bodies = funBodies s0
--   case M.lookup name bodies of
--     Just _  -> error $ unwords $ ["Already defined", name]
--     Nothing -> do
--       put $ s0 { funBodies = M.insert name body bodies
--                , funTypes = M.insert name retType $ funTypes s0
--                , funArgTypes = M.insert name argTypes $ funArgTypes s0
--                }

-- getFunctionType :: String -> Codegen Type
-- getFunctionType name = do
--   (_,_,ty,_) <- getFunction name
--   return ty

-- getFunction :: String
--             -> Codegen ([Codegen Stmt], [(String, Type)], Type, VNode)
-- getFunction name = do
--   s0 <- get
--   let body = case M.lookup name $ funBodies s0 of
--                Just bod -> bod
--                Nothing -> error $ unwords ["No function body defined for", name]
--       args = case M.lookup name $ funArgTypes s0 of
--                Just as -> as
--                Nothing -> error $ unwords ["No args listed for", name]
--       rett = case M.lookup name $ funTypes s0 of
--                Just rt -> rt
--                Nothing -> error $ unwords ["No return value for", name]
--       rvs = case M.lookup name $ funRetVals s0 of
--               Nothing   -> []
--               Just rvs' -> rvs'
--   rv <- liftVerif $ newResultVar rett (name ++ "_return_" ++ show (length rvs))
--   put $ s0 { funRetVals = M.insert name (rv:rvs) $ funRetVals s0 }
--   return (body, args, rett, rv)

-- getRv :: String
--       -> Codegen (Maybe VNode)
-- getRv name = do
--   rvs <- funRetVals `liftM` get
--   case M.lookup name rvs of
--     Just vs -> return $ Just $ head vs
--     Nothing -> return Nothing

-- -- | Make (and return) a new variable
-- newVar :: Type -> Variable -> Codegen ()
-- newVar ty var = do
--   s0 <- get
--   let allVars = vars s0
--   if M.member var allVars
--   then return ()
--   else put $ s0 { vars = M.insert var [] allVars
--                 , tys = M.insert var ty $ tys s0
--                 }

-- -- | Get the most recent version of a variable
-- curVar :: Variable -> Codegen (VNode, Version)
-- curVar var = do
--   allVars <- vars `liftM` get
--   case M.lookup var allVars of
--     Nothing -> error $ unwords [var, "has not been declared"]
--     Just vs -> do
--       if null vs
--       then error $ unwords [var, "has no versions at all"]
--       else return $ (head vs, length vs - 1)

-- -- | Get the ver version of a variable
-- varVer :: Variable -> Int -> Codegen VNode
-- varVer var ver = do
--   allVars <- vars `liftM` get
--   case M.lookup var allVars of
--     Nothing -> error $ unwords [var, "has not been declared"]
--     Just vs -> if length vs > ver
--                then return $ (reverse vs) !! ver
--                else error $ unwords [var, "has no version", show ver]

-- -- | Get an increased version of the variable
-- nextVer :: Variable -> Codegen (VNode, Version)
-- nextVer var = do
--   s0 <- get
--   let allVars = vars s0
--   case M.lookup var allVars of
--     Nothing -> error $ unwords [var, "has not been declared"]
--     Just vs -> if null vs
--                then do
--                  let ty = tys s0 M.! var
--                  newVar <- liftVerif $ newResultVar ty $ var ++ "_0"
--                  put $ s0 { vars = M.insert var (newVar:vs) allVars }
--                  return (newVar, 0)
--                else do
--                  let ty = vtype $ head vs
--                      ver = length vs
--                  newVar <- liftVerif $ newResultVar ty $ var ++ "_" ++ show ver
--                  put $ s0 { vars = M.insert var (newVar:vs) allVars }
--                  return (newVar, ver)
