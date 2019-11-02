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
    T.Bool       -> if val == 1 then T.true else T.false
    _            -> error "Float not supported"

genCallSMT :: SExpr
           -> Codegen [T.VNode]
genCallSMT expr =
  case expr of
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

      unless (length argSyms == length formalArgSyms) $
        error $ unwords ["Improper number of arguments to", name]
      -- Set the arguments equal to the formal arguments
      forM_ (zip (concat argSyms) (concat formalArgSyms)) $
        \(a, f) -> liftVerif $ T.vassign a f

      lazyBodyStmts <- getBody name

      let newClassVar = exprVar $ head args
      setClassVar newClassVar

      retValSyms <- getReturnVal name >>= mapM getVar
      setReturnValue retValSyms

      -- Execute the function. This will re-version all the variables in the function
      -- Then, generate SMT for the function (and provide the function with the return
      -- value, so it can properly assign return statements)
      forM_ lazyBodyStmts $ \line' -> do
                              line <- line'    -- Re-version everything
                              genStmtSMT line  -- SMT the line. The line has access to the
                                               -- the function's return value and possible
                                               -- class var via the underlying monad

      clearClassVar
      clearRetVal
      return retValSyms

    _ -> error "Cannot generate call SMT for non-call node"

genBinOpSMT :: SExpr
            -> SExpr
            -> (T.VNode -> T.VNode -> T.Verif T.VNode)
            -> Codegen T.VNode
genBinOpSMT e1 e2 op = do
  v1 <- genExprSMT e1
  v2 <- genExprSMT e2
  liftVerif $ op v1 v2

genExprSMT :: SExpr
           -> Codegen T.VNode
genExprSMT expr =
  case expr of
    VarExpr svar           -> genVarSMT svar
    NumExpr snum           -> genNumSMT snum
    Tern c b1 b2 -> do
      cSym <- genExprSMT c
      b1Sym <- genExprSMT b1
      b2Sym <- genExprSMT b2
      liftVerif $ T.cppCond cSym b1Sym b2Sym
    Cast expr ty           -> do
      exprSMT <- genExprSMT expr
      liftVerif $ T.cppCast exprSMT ty
    FieldExpr fieldName    -> do
      cv <- getClassVar
      case cv of
        Nothing  -> error "Cannot do plain field access outside of class method"
        Just var -> getField var fieldName >>= getVar

    -- Unary operators
    Neg e -> genExprSMT e >>= liftVerif . T.cppNeg
    Not e -> genExprSMT e >>= liftVerif . T.cppNot
    Abs e -> genExprSMT e >>= liftVerif . T.cppAbs
    -- Binary
    Eq left right  -> genBinOpSMT left right T.cppEq
    And left right -> genBinOpSMT left right T.cppAnd
    Add left right -> genBinOpSMT left right T.cppAdd
    Sub left right -> genBinOpSMT left right T.cppSub
    Mul left right -> genBinOpSMT left right T.cppMul
    Or left right  -> genBinOpSMT left right T.cppOr
    XOr left right -> genBinOpSMT left right T.cppXor
    Min left right -> genBinOpSMT left right T.cppMin
    Max left right -> genBinOpSMT left right T.cppMax
    Gt left right  -> genBinOpSMT left right T.cppGt
    Gte left right -> genBinOpSMT left right T.cppGte
    Lt left right  -> genBinOpSMT left right T.cppLt
    Lte left right -> genBinOpSMT left right T.cppLte
    Shl left right -> genBinOpSMT left right T.cppShiftLeft
    Shr left right -> genBinOpSMT left right T.cppShiftRight

    Call{}         -> do
      result <- genCallSMT expr
      case result of
        [rval] -> return rval
        _      -> error "Cannot return a class variable for use in greater expression"

genStmtSMT :: SStmt -> Codegen ()
genStmtSMT stmt =
  case stmt of
    Decl var -> return () -- Declaration is just important for variable tracking
    Assert e -> genExprSMT e >>= liftVerif . T.vassert
    -- We are assigning to a class
    -- In this case, the RHS must either be another class or a call
    Assign (VarExpr var) expr | isClassType var ->
      case expr of
        -- Assign each field of the LHS struct to each return value
        Call{}    -> do
          rvs <- genCallSMT expr
          fields <- getFieldVars var >>= mapM getVar
          unless (length rvs == length fields) $
            error $ unwords ["Wrong return type for function in assignment to", varName var]
          forM_ (zip fields rvs) $ \(f, rv) -> liftVerif $ T.vassign f rv
        -- Assign each field of the RHS struct to the LHS struct
        VarExpr v | isClassType v -> do
          fields1 <- getFieldVars var >>= mapM getVar
          fields2 <- getFieldVars v >>= mapM getVar
          unless (length fields1 == length fields2) $
            error $ unwords ["Type error in assignment to", varName var]
          forM_ (zip fields1 fields2) $ \(f, r) -> liftVerif $ T.vassign f r
        _ -> error $ unwords $ ["Malformed assignment to", varName var]
    Assign var expr -> do
      varSMT <- genExprSMT var
      exprSMT <- genExprSMT expr
      liftVerif $ T.vassign varSMT exprSMT
    If cond trueBr falseBr -> do
      condSym <- genExprSMT cond
      mapM_ (rewriteConditional condSym) trueBr
      notCond <- liftVerif $ T.cppNot condSym
      mapM_ (rewriteConditional notCond) falseBr
    Return expr -> do
      rv <- getReturnValue
      case rv of
        [] -> return ()
        [rval] -> do
          exprSym <- genExprSMT expr
          liftVerif $ T.vassign rval exprSym
        rvals -> do
          fields <- case expr of
                      _ | isClassExpr expr -> getFieldVars (exprVar expr) >>= mapM getVar
                      _ | isCallExpr expr -> genCallSMT expr
                      _ -> error $ unwords ["Malfored return expression", show expr]
          unless (length fields == length rvals) $
            error "Cannot assign variables of different types"
          forM_ (zip fields rvals) $ \(f, r) -> liftVerif $ T.vassign f r
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
    rewriteReturn :: T.VNode -> T.VNode -> T.VNode -> Codegen ()
    rewriteReturn cond retVal exprVal = do
      conditionalFalse <- liftVerif $ T.cppNot cond
      rvalIsExpr <- liftVerif $ T.cppEq retVal exprVal
      liftVerif $ T.cppOr conditionalFalse rvalIsExpr >>= T.vassert
    -- Guard each assignment with the given condition
    rewriteConditional :: T.VNode -> SStmt -> Codegen ()
    rewriteConditional cond stmt =
      case stmt of
        If cond2 trueBr falseBr -> do
          condSMT <- genExprSMT cond2
          bothConds <- liftVerif $ T.cppAnd cond condSMT
          mapM_ (rewriteConditional bothConds) trueBr
          notBothConds <- liftVerif $ T.cppNot bothConds
          mapM_ (rewriteConditional notBothConds) falseBr
        Assign (VarExpr var) expr ->
          if isPrimType var
          then rewriteAssign cond var expr
          else do
            fieldVars <- getFieldVars var
            forM_ fieldVars $ \var -> rewriteAssign cond var expr
        Return expr -> do
          rv <- getReturnValue
          case rv of
            [] -> return ()
            [rval] -> do
              exprSym <- genExprSMT expr
              rewriteReturn cond rval exprSym
            rvals -> do
              fields <- case expr of
                          _ | isClassExpr expr -> getFieldVars (exprVar expr) >>= mapM getVar
                          _ | isCallExpr expr -> genCallSMT expr
                          _ -> error $ unwords ["Malfored return expression", show expr]
              unless (length fields == length rvals) $
                error "Cannot assign variables of different types"
              forM_ (zip fields rvals) $ \(f, r) -> rewriteReturn cond f r
        _ -> genStmtSMT stmt

genBodySMT :: [Codegen SStmt] -> Codegen ()
genBodySMT body = forM_ body $ \line -> line >>= genStmtSMT

listOf :: a -> [a]
listOf x = [x]
