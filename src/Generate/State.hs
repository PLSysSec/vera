{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Generate.State where
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           DSL.DSL                    hiding (vars)
import           DSL.Typed
import           Generate.SMTAST
import qualified Z3.Monad                   as Z

data CodegenState = CodegenState { vars      :: M.Map VarName Version
                                 , tys       :: M.Map VarName STy
                                 , classes   :: M.Map ClassName (M.Map FieldName Type)
                                 -- * For code generation
                                 , syms      :: M.Map SVar VNode
                                 , functions :: M.Map FunctionName LazyFunction
                                 }

-- | A function action that we can demonadify at will.
-- When we use it, it will automatically version all the variables in
-- the function, as well as creating a new return value to the function
data LazyFunction = LazyFunction { formalArgs   :: [VarName]
                                 , functionBody :: [Codegen SStmt]
                                 , returnVal    :: VarName
                                 }

newtype Codegen a = Codegen (StateT CodegenState Verif a)
    deriving (Functor, Applicative, Monad, MonadState CodegenState, MonadIO)

instance Z.MonadZ3 Codegen where
    getSolver = Codegen $ lift $ Z.getSolver
    getContext = Codegen $ lift $ Z.getContext

emptyCodegenState :: CodegenState
emptyCodegenState = CodegenState M.empty M.empty M.empty M.empty M.empty

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

addFunction :: FunctionName -> [VarName] -> VarName -> [Codegen SStmt] -> Codegen ()
addFunction funName funArgs retVal body = do
  s0 <- get
  case M.lookup funName $ functions s0 of
    Just fun -> error $ unwords ["Already defined function", funName]
    Nothing  -> do
      let fun = LazyFunction funArgs body retVal
      put $ s0 { functions = M.insert funName fun $ functions s0 }

getFormalArgs :: FunctionName -> Codegen [SVar]
getFormalArgs funName = do
  s0 <- get
  case M.lookup funName $ functions s0 of
    Just (LazyFunction args _ _) -> forM args nextVar
    Nothing  -> error $ unwords ["Function", funName, "undefined so has no formal args"]

getReturnVal :: FunctionName -> Codegen SVar
getReturnVal funName = do
  s0 <- get
  case M.lookup funName $ functions s0 of
    Just (LazyFunction _ _ rv) -> nextVar rv
    Nothing -> error $ unwords ["Function", funName, "undefined so has no return value"]

getBody :: FunctionName -> Codegen [Codegen SStmt]
getBody funName = do
  s0 <- get
  case M.lookup funName $ functions s0 of
    Just (LazyFunction _ body _) -> return body
    Nothing -> error $ unwords ["Function", funName, "undefined so has no body"]

addClass :: ClassName -> M.Map FieldName Type -> Codegen ()
addClass className fields = do
  s0 <- get
  case M.lookup className $ classes s0 of
    Nothing -> put $ s0 { classes = M.insert className fields $ classes s0 }
    _       -> error $ unwords $ ["Class", className, "already declared"]

getFields :: ClassName -> Codegen (M.Map FieldName Type)
getFields name = do
  s0 <- get
  case M.lookup name $ classes s0 of
    Nothing     -> error $ unwords ["Class", name, "undeclared"]
    Just fields -> return fields

getVarOrFields :: SVar -> Codegen [VNode]
getVarOrFields var = case varTy var of
                       Class c -> do
                         fieldTys <- getFields c
                         forM (M.toList fieldTys) $ \(name, _) ->
                           curVar (varName var ++ "_" ++ name) >>= getVar
                       _       -> do
                         node <- getVar var
                         return [node]

getVar :: SVar -> Codegen VNode
getVar var = do
  unless (isPrimType var) $
    error $ unwords $ ["Cannot make symbolic class variable", varName var]
  s0 <- get
  let allSyms = syms s0
  case M.lookup var allSyms of
    Just sym -> return sym
    Nothing -> do
      let name = (varName var) ++ "_" ++ (show $ varVersion var)
      sym <- liftVerif $ newResultVar (primTy $ varTy var) name
      put $ s0 { syms = M.insert var sym allSyms }
      return sym

newVar :: STy -> String -> Codegen ()
newVar ty str = do
  varsToMake <- case ty of
    PrimType pt -> return [(str, PrimType pt)]
    Class c     -> do
      fields <- getFields c
      fvs <- forM (M.toList fields) $ \(name, ty) -> return (str ++ "_" ++ name, PrimType ty)
      return $ (str, ty):fvs
  forM_ varsToMake $ \(v, t) -> addVar v t
  where
    addVar :: VarName -> STy -> Codegen ()
    addVar var ty = do
      s0 <- get
      let allVars = vars s0
          allTys = tys s0
      case M.lookup var allVars of
        Just{} -> return ()
        Nothing -> put $ s0 { vars = M.insert var 0 allVars
                            , tys = M.insert var ty allTys
                            }

varType :: VarName -> Codegen STy
varType str = do
  allTys <- tys `liftM` get
  case M.lookup str allTys of
    Nothing -> error $ unwords $ ["Undefined var", str]
    Just ty -> return ty

curVersion :: String -> Codegen Int
curVersion str = do
  s0 <- get
  case M.lookup str $ vars s0 of
    Nothing -> error $ unwords ["Undeclared variable", str]
    Just v  -> return v

curVar :: String -> Codegen SVar
curVar str = do
  ty <- varType str
  ver <- curVersion str
  return $ SVar ty str ver

nextVar :: String -> Codegen SVar
nextVar str = do
  var <- curVar str
  nextVer <- nextVersion str
  return $ setVersion var nextVer

nextVersion :: String -> Codegen Int
nextVersion str = do
  ty <- varType str
  case ty of
    Class c -> do
      fields <- getFields c
      forM_ (M.toList fields) $ \(name, _) -> updateVersion $ str ++ "_" ++ name
      updateVersion str
    _       -> updateVersion str
  where
    updateVersion :: String -> Codegen Int
    updateVersion str = do
      s0 <- get
      case M.lookup str $ vars s0 of
        Nothing -> error $ unwords ["Undeclared variable", str]
        Just v  -> do
          let nextVer = v + 1
          put $ s0 { vars = M.insert str nextVer $ vars s0 }
          return nextVer



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
