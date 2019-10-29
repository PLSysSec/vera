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

--
-- Normal setup things
--

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
-- Functions
--

-- | Make a new LazyFunction. Anytime we invoke it, it will re-version all of the
-- variables within the function body automatically
addFunction :: FunctionName -> [VarName] -> VarName -> [Codegen SStmt] -> Codegen ()
addFunction funName funArgs retVal body = do
  s0 <- get
  case M.lookup funName $ functions s0 of
    Just fun -> error $ unwords ["Already defined function", funName]
    Nothing  -> do
      let fun = LazyFunction funArgs body retVal
      put $ s0 { functions = M.insert funName fun $ functions s0 }

-- | Return the formal arguments to a function
getFormalArgs :: FunctionName -> Codegen [SVar]
getFormalArgs funName = do
  s0 <- get
  case M.lookup funName $ functions s0 of
    Just (LazyFunction args _ _) -> forM args nextVar
    Nothing  -> error $ unwords ["Function", funName, "undefined so has no formal args"]

-- | Return the variable representing a function's return value
getReturnVal :: FunctionName -> Codegen SVar
getReturnVal funName = do
  s0 <- get
  case M.lookup funName $ functions s0 of
    Just (LazyFunction _ _ rv) -> nextVar rv
    Nothing -> error $ unwords ["Function", funName, "undefined so has no return value"]

-- | Return the body of a function
getBody :: FunctionName -> Codegen [Codegen SStmt]
getBody funName = do
  s0 <- get
  case M.lookup funName $ functions s0 of
    Just (LazyFunction _ body _) -> return body
    Nothing -> error $ unwords ["Function", funName, "undefined so has no body"]

--
-- Classes
--

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
getVarOrFields var = case var of
                       CVar c _ -> do
                         fieldTys <- getFields c
                         forM (M.toList fieldTys) $ \(name, _) ->
                           curVar (varName var ++ "_" ++ name) >>= getVar
                       _       -> do
                         node <- getVar var
                         return [node]

--
-- Variables
--

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
      sym <- liftVerif $ newResultVar (varTy var) name
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

--
-- Variables
--

varType :: VarName -> Codegen STy
varType str = do
  allTys <- tys `liftM` get
  case M.lookup str allTys of
    Just ty -> return ty
    Nothing -> error $ unwords $ ["Undeclared variable:", str]

curVar :: String -> Codegen SVar
curVar str = do
  ty <- varType str
  if isClass ty
  then return $ CVar (className ty) str
  else do
    s0 <- get
    case M.lookup str $ vars s0 of
      Nothing  -> error $ unwords ["Undeclared variable", str]
      Just ver -> return $ SVar (primTy ty) str ver

nextVar :: String -> Codegen SVar
nextVar str = do
  ty <- varType str
  case ty of
    Class c -> do
      fields <- getFields c
      forM_ (M.toList fields) $ \(name, _) -> updateVersion $ str ++ "_" ++ name
      return $ CVar c str
    _       -> do
      ver <- updateVersion str
      return $ SVar (primTy ty) str ver
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
