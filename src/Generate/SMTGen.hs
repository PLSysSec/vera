module Generate.SMTGen where
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import qualified DSL.DSL                    as D
import qualified DSL.Typed                  as T
import           Generate.SMTAST
import           Generate.State

genVarSMT :: SVar
           -> Codegen T.VNode
genVarSMT var = getVar var

genNumSMT :: SNum
          -> Codegen T.VNode
genNumSMT snum = do
  liftVerif $ case numTy snum of
    T.Signed16   -> T.num16 val
    T.Unsigned16 -> T.unum16 val
    T.Signed     -> T.num val
    T.Unsigned   -> T.unum val
    T.Signed64   -> T.num64 val
    T.Unsigned64 -> T.unum64 val
    T.Bool       -> if val == 1 then T.true else T.false
    T.Double     -> T.fpnum fval
    _            -> error "Float not supported"
  where
    val = numVal snum
    fval = floatVal snum

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
        forM fas $ \fa ->
          if isPrimType fa
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
    NEq left right -> genBinOpSMT left right (\l r -> T.cppEq l r >>= T.cppNot)
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
    -- Weird
    Undef var         -> do
      result <- genExprSMT var
      let resultUndef = T.vundef result
      liftVerif $ T.newDefinedNode resultUndef T.Bool
    -- JavaScript
    JSAnd left right  -> genBinOpSMT left right T.jsAnd
    JSAdd left right  -> genBinOpSMT left right T.jsAdd
    JSSub left right  -> genBinOpSMT left right T.jsSub
    JSMul left right  -> genBinOpSMT left right T.jsMul
    JSOr  left right  -> genBinOpSMT left right T.jsOr
    JSXOr left right  -> genBinOpSMT left right T.jsXor
    JSMin left right  -> genBinOpSMT left right T.jsMin
    JSMax left right  -> genBinOpSMT left right T.jsMax
    JSLsh left right  -> genBinOpSMT left right T.jsShl
    JSRsh left right  -> genBinOpSMT left right T.jsShr
    JSUrsh left right -> genBinOpSMT left right T.jsUshr
    JSRem left right  -> genBinOpSMT left right T.jsRem
    JSDiv  left right -> genBinOpSMT left right T.jsDiv
    JSAbs e           -> genExprSMT e >>= liftVerif . T.jsAbs
    JSNot e           -> genExprSMT e >>= liftVerif . T.jsNot
    JSCeil e          -> genExprSMT e >>= liftVerif . T.jsCeil
    JSFloor e         -> genExprSMT e >>= liftVerif . T.jsFloor
    JSSign  e         -> genExprSMT e >>= liftVerif . T.jsSign
    -- Fp
    IsInf  e     -> genExprSMT e >>= liftVerif . T.isInf
    IsNan  e     -> genExprSMT e >>= liftVerif . T.isNan
    IsZero e     -> genExprSMT e >>= liftVerif . T.isZero
    IsNegative e -> genExprSMT e >>= liftVerif . T.isNeg
    IsNegativeZero  e -> do
      exp <- genExprSMT e
      isZero <- liftVerif $ T.isZero exp
      isNeg <- liftVerif $ T.isNeg exp
      liftVerif $ T.cppAnd isZero isNeg
    GetExp e     -> genExprSMT e >>= liftVerif . T.getFpExponent

    Call{}         -> do
      result <- genCallSMT expr
      case result of
        [rval] -> return rval
        _      -> error $ unwords ["Cannot return a class variable for use in greater expression", show result]
    _ -> error $ unwords $ ["Don't support", show expr]

genAssignOpSMT :: SExpr
               -> SExpr
               -> SExpr
               -> (T.VNode -> T.VNode -> T.Verif T.VNode)
               -> Codegen ()
genAssignOpSMT result one two op = do
  lhsSMT <- genExprSMT result
  leftSMT <- genExprSMT one
  rightSMT <- genExprSMT two
  tmp <- liftVerif $ op leftSMT rightSMT
  liftVerif $ T.vassign lhsSMT tmp

genStmtSMT :: SStmt -> Codegen ()
genStmtSMT stmt =
  case stmt of
    Expect resultPred act -> do
      result <- runSolverOnSMT
      unless (resultPred result) $ liftIO $ act result
    Push -> D.push
    Pop -> D.pop
    Assert e -> genExprSMT e >>= liftVerif . T.vassert
    VoidCall name expr -> void $ genCallSMT $ Call name expr
    Decl var -> return () -- Declaration is just important for variable tracking
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

    AddEq result one two -> genAssignOpSMT result one two T.cppAdd
    SubEq result one two -> genAssignOpSMT result one two T.cppSub
    OrEq result one two  -> genAssignOpSMT result one two T.cppOr
    AndEq result one two -> genAssignOpSMT result one two T.cppAnd

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
    rewriteAssign :: T.VNode -> SVar -> T.VNode -> Codegen ()
    rewriteAssign cond var exprNode | isPrimType var = do
      when (varVersion var <= 1) $
        error $ unwords $ ["Cannot initially define var in if-stmt:", varName var]
      curVar <- genVarSMT var
      let prevVar = SVar (varTy var) (varName var) (varVersion var - 1)
          trueBr = exprNode
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
          notBothConds <- do
            notCond2 <- liftVerif $ T.cppNot condSMT
            liftVerif $ T.cppAnd cond notCond2
          mapM_ (rewriteConditional notBothConds) falseBr
        Assign (VarExpr var) expr -> do
          if isPrimType var
          then do
            exprSMT <- genExprSMT expr
            rewriteAssign cond var exprSMT
          else case expr of
            Call{} -> do
              rvs <- genCallSMT expr
              fields <- getFieldVars var
              unless (length rvs == length fields) $
                error $ unwords ["Wrong return type for function in assignment to", varName var]
              forM_ (zip fields rvs) $ \(f, rv) -> rewriteAssign cond f rv
            VarExpr v | isClassType v -> do
              fields1 <- getFieldVars var
              fields2 <- getFieldVars v >>= mapM genVarSMT
              unless (length fields1 == length fields2) $
                error $ unwords ["Type error in assignment to", varName var]
              forM_ (zip fields1 fields2) $ \(f, r) -> rewriteAssign cond f r
            _ -> error $ unwords $ ["Malformed assignment to", varName var]

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
