module Generate.Lang where
import           Control.Monad              (unless)
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           Data.Maybe                 (catMaybes, fromJust, isJust)
import qualified DSL.DSL                    as D
import           DSL.Typed
import           Generate.SMTAST
import           Generate.State

data FunctionDef = Function { fName :: FunctionName
                            , fTy   :: STy
                            , fArgs :: [(VarName, STy)]
                            , fBody :: [Codegen SStmt]
                            }

data ClassDef = ClassDef ClassName [(FieldName, Type)] [FunctionDef]

data Program = Program [FunctionDef] [ClassDef]

--
-- Top-level declarations and definitions
--

program :: [FunctionDef]
        -> [ClassDef]
        -> Codegen Program
program fns classes = do
  forM_ fns define
  forM_ classes class_
  return $ Program fns classes

define :: FunctionDef
       -> Codegen ()
define (Function funName funTy funArgs body) = do
  -- Declare all the argument variables and the return value
  forM_ funArgs $ \(name, ty) -> newVar ty name
  let retValName = funName ++ "_return_val"
  newVar funTy retValName
  rvs <- if isClass funTy
         then do
           retVal <- curVar retValName
           fields <- getFieldVars retVal
           return $ map varName fields
         else return $ if isVoid funTy then [] else [retValName]
  -- Save the relevant information in the state so we can call it later
  addFunction funName (map fst funArgs) rvs body

class_ :: ClassDef -> Codegen ()
class_ (ClassDef name fields functions) = do
  addClass name $ M.fromList fields
  forM_ functions $ \(Function funName funTy funArgs body) -> do
    let newName = name ++ "_" ++ funName
        classArg = (newName ++ "_arg", Class name)
    define $ Function newName funTy (classArg:funArgs) body

--
-- Variables and numbers
--

-- | Make a primitive type
t :: Type -> STy
t = PrimType

-- | Make a class type
c :: String -> STy
c = Class

-- | Get a declared variable
v :: VarName -> Codegen SExpr
v name = do
  ty <- varType name
  if isClass ty
  then return $ VarExpr $ CVar (className ty) name
  else curVar name >>= return . VarExpr

-- | Get a number
n :: Type -> Integer -> Codegen SExpr
n ty num = return $ NumExpr $ SNum ty num

-- | Get an fp number
d :: Type -> Double -> Codegen SExpr
d ty num = case ty of
             Double -> return $ NumExpr $ FNum ty num
             _      -> error "Cannot make non-float float "

-- | Get field from field name in a class method
f :: FieldName -> Codegen SExpr
f name = return $ FieldExpr name

--
-- Operators
--

cast :: Codegen SExpr
     -> Type
     -> Codegen SExpr
cast expr' ty = do
  expr <- expr'
  return $ Cast expr ty

call :: String
     -> [Codegen SExpr]
     -> Codegen SExpr
call name args' = do
  args <- forM args' $ \arg -> arg
  return $ Call name args

vcall :: String
      -> [Codegen SExpr]
      -> Codegen SStmt
vcall name args' = do
  args <- forM args' $ \arg -> arg
  return $ VoidCall name args

binOp :: Codegen SExpr -> Codegen SExpr -> (SExpr -> SExpr -> SExpr) -> Codegen SExpr
binOp left' right' op = do
  left <- left'
  right <- right'
  return $ op left right

-- we have this for when we add type checking
unaryOp :: Codegen SExpr -> (SExpr -> SExpr) -> Codegen SExpr
unaryOp ex' op = do
  ex <- ex'
  return $ op ex

tern_ :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr -> Codegen SExpr
tern_ c' b1' b2' = do
  c <- c'
  b1 <- b1'
  b2 <- b2'
  return $ Tern c b1 b2



neg_ :: Codegen SExpr -> Codegen SExpr
neg_ ex = unaryOp ex Neg

not_ :: Codegen SExpr -> Codegen SExpr
not_ ex = unaryOp ex Not

jsNot :: Codegen SExpr -> Codegen SExpr
jsNot ex = unaryOp ex JSNot

abs_ :: Codegen SExpr -> Codegen SExpr
abs_ ex = unaryOp ex Abs

jsAbs :: Codegen SExpr -> Codegen SExpr
jsAbs ex = unaryOp ex JSAbs

jsSign :: Codegen SExpr -> Codegen SExpr
jsSign ex = unaryOp ex JSSign

jsCeil :: Codegen SExpr -> Codegen SExpr
jsCeil ex = unaryOp ex JSCeil

jsFloor :: Codegen SExpr -> Codegen SExpr
jsFloor ex = unaryOp ex JSFloor

isNan :: Codegen SExpr -> Codegen SExpr
isNan op = op >>= return . IsNan

isInf :: Codegen SExpr -> Codegen SExpr
isInf op = op >>= return . IsInf

isZero :: Codegen SExpr -> Codegen SExpr
isZero op = op >>= return . IsZero

isNeg :: Codegen SExpr -> Codegen SExpr
isNeg op = op >>= return . IsNegative

fpExp :: Codegen SExpr -> Codegen SExpr
fpExp op = op >>= return . GetExp

(.==.) :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
(.==.) left right = binOp left right Eq

(.!=.) :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
(.!=.) left right = binOp left right NEq

(.&&.) :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
(.&&.) left right = binOp left right And

(.&=.) :: Codegen SExpr -> Codegen SExpr -> Codegen SStmt
(.&=.) left right = assignOp left right AndEq

jsAnd :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
jsAnd left right = binOp left right JSAnd

(.+.) :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
(.+.) left right = binOp left right Add

(.+=.) :: Codegen SExpr -> Codegen SExpr -> Codegen SStmt
(.+=.) left right = assignOp left right AddEq

jsAdd :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
jsAdd left right = binOp left right JSAdd

(.-.) :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
(.-.) left right = binOp left right Sub

(.-=.) :: Codegen SExpr -> Codegen SExpr -> Codegen SStmt
(.-=.) left right = assignOp left right SubEq

jsSub :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
jsSub left right = binOp left right JSSub

(.*.) :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
(.*.) left right = binOp left right Mul

jsMul :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
jsMul left right = binOp left right JSMul

(.||.) :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
(.||.) left right = binOp left right Or

(.|=.) :: Codegen SExpr -> Codegen SExpr -> Codegen SStmt
(.|=.) left right = assignOp left right OrEq

jsOr :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
jsOr left right = binOp left right JSOr

(.^.) :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
(.^.) left right = binOp left right XOr

jsXOr :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
jsXOr left right = binOp left right JSXOr

min_ :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
min_ left right = binOp left right Min

jsMin :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
jsMin left right = binOp left right JSMin

max_ :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
max_ left right = binOp left right Max

jsMax :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
jsMax left right = binOp left right JSMax

(.>.) :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
(.>.) left right = binOp left right Gt

(.=>.) :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
(.=>.) left right = binOp left right Gte

(.<.) :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
(.<.) left right = binOp left right Lt

(.<=.) :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
(.<=.) left right = binOp left right Lte

(.<<.) :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
(.<<.) left right = binOp left right Shl

jsLsh :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
jsLsh left right = binOp left right JSLsh

(.>>.) :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
(.>>.) left right = binOp left right Shr

jsRsh :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
jsRsh left right = binOp left right JSRsh

jsUrsh :: Codegen SExpr -> Codegen SExpr -> Codegen SExpr
jsUrsh left right = binOp left right JSUrsh

(.->.) :: Codegen SExpr -> FieldName -> Codegen SExpr
(.->.) ve' fieldname = do
  ve <- ve'
  unless (isClassExpr ve) $ error $ unwords ["Cannot get field of non class", show ve]
  getField (exprVar ve) fieldname >>= return . VarExpr

method :: Codegen SExpr -> FunctionName -> [Codegen SExpr] -> Codegen SExpr
method ve' funName args' = do
  ve <- ve'
  args <- forM args' $ \arg -> arg
  unless (isClassExpr ve) $ error $ unwords ["Cannot get method of non class", show ve]
  let fullFunName = (varClass $ exprVar ve) ++ "_" ++ funName
  return $ Call fullFunName (ve:args)

--
-- Statements
--

declare :: STy -> VarName -> Codegen SStmt
declare ty var = do
  newVar ty var
  return $ if isClass ty
  then Decl $ CVar (className ty) var
  else Decl $ SVar (primTy ty) var 0
declare _ _ = error "Class type is not set up yet"

assign :: Codegen SExpr
       -> Codegen SExpr
       -> Codegen SStmt
assign svar' sexpr' = do
  svar <- svar'
  sexpr <- sexpr'
  unless (isClassExpr svar || isPrimVarExpr svar) $ error "Cannot assign to non-variable"
  when (isClassExpr svar) $ unless (isClassExpr sexpr || isCallExpr sexpr) $
    error "Cannot assign class to non-class"
  newVar <- nextVar (varName $ exprVar svar)
  return $ Assign (VarExpr newVar) sexpr

assignOp :: Codegen SExpr
         -> Codegen SExpr
         -> (SExpr -> SExpr -> SExpr -> SStmt)
         -> Codegen SStmt
assignOp svar' sexpr' op = do
  svar <- svar'
  sexpr <- sexpr'
  unless (isPrimVarExpr svar) $ error "Cannot update-assign to a non-number-variable"
  newVar <- nextVar (varName $ exprVar svar)
  return $ op (VarExpr newVar) svar sexpr

if_ :: Codegen SExpr
    -> [Codegen SStmt]
    -> [Codegen SStmt]
    -> Codegen SStmt
if_ cond' ifBr' elseBr' = do
  cond <- cond'
  ifBr <- forM ifBr' $ \line -> line
  elseBr <- forM elseBr' $ \line -> line
  return $ If cond ifBr elseBr

return_ :: Codegen SExpr
        -> Codegen SStmt
return_ expr' = do
  expr <- expr'
  return $ Return expr

--
-- SMT directives
--

assert_ :: Codegen SExpr
        -> Codegen SStmt
assert_ expr' = do
  expr <- expr'
  return $ Assert expr

expect_ :: (SMTResult -> Bool)
        -> (SMTResult -> IO ())
        -> Codegen SStmt
expect_ result act = return $ Expect result act

push_ :: Codegen SStmt
push_ = return Push

pop_ :: Codegen SStmt
pop_ = return Pop
