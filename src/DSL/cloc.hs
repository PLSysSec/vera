
                 , jsAdd
                 , jsSub
                 , jsAnd
                 , jsOr
                 , jsXor
                 , jsNot
                 , jsShl
                 , jsShr
                 , jsUshr
                 , jsMul
                 , jsDiv
                 , jsRem
                 , jsAbs
                 , jsMin
                 , jsMax
                 , jsSign
                 , jsFloor
                 , jsCeil


    getOp :: VNode
      -> (D.AST -> D.AST -> D.Verif D.AST)
      -> (D.AST -> D.AST -> D.Verif D.AST)
      -> (D.AST -> D.AST -> D.Verif D.AST)         
getOp representativeNode intOp fpOp =
  case numBits representativeNode of 
    32   -> intOp
    1000 -> fpOp
    _    -> error "JS operations only support ints or fp ops"

jsAdd :: VNode -> VNode -> D.Verif VNode
jsAdd node1 node2 = do
  unless (numBits node1 == numBits node2) $ error "Widths should match in jsAdd"
  let op = getOp node1 D.add D.fpAdd
  result <- op (vnode node1) (vnode node2)
  newDefinedNode result $ vtype node1

jsSub :: VNode -> VNode -> D.Verif VNode
jsSub node1 node2 = do
  unless (numBits node1 == numBits node2) $ error "Widths should match in jsSub"
  let op = getOp node1 D.sub D.fpSub
  result <- op (vnode node1) (vnode node2)
  newDefinedNode result $ vtype node1                 

jsAnd :: VNode -> VNode -> D.Verif VNode
jsAnd node1 node2 = do
  unless (numBits node1 == numBits node2) $ error "Widths should match in jsAnd"
  unless (is32Bits $ vtype node1) $ error "JavaScript AND does not support floats"
  result <- D.and (vnode node1) (vnode node2)
  newDefinedNode result $ vtype node1

jsOr :: VNode -> VNode -> D.Verif VNode
jsOr node1 node2 = do
  unless (numBits node1 == numBits node2) $ error "Widths should match in jsOr"
  unless (is32Bits $ vtype node1) $ error "JavaScript OR does not support floats"         
  result <- D.or (vnode node1) (vnode node2)
  newDefinedNode result $ vtype node1

jsXor :: VNode -> VNode -> D.Verif VNode
jsXor node1 node2 = do
  unless (numBits node1 == numBits node2) $ error "Widths should match in jsXor"
  unless (is32Bits $ vtype node1) $ error "Javascripts Xor does not support floats"
  result <- D.xor (vnode node1) (vnode node2)
  newDefinedNode result $ vtype node1
  
jsNot :: VNode -> D.Verif VNode
jsNot node = do
  unless (is32Bits $ vtype node) $ error "JavaScript NOT does not support floats"  
  result <- D.not (vnode node)
  newDefinedNode result $ vtype node

-- | https://es5.github.io/#x11.7.1
--
-- Let lnum be ToInt32(lval).
--
-- Let rnum be ToUint32(rval).
--
-- Let shiftCount be the result of masking out all but the least significant 5 bits of rnum,
-- that is, compute rnum & 0x1F.
--
--  Return the result of left shifting lnum by shiftCount bits. The result is a signed
-- 32-bit integer.
jsShl :: VNode
      -> VNode
      -> D.Verif VNode
jsShl left right = do
  unless (numBits left == numBits right) $ error "Widths should match in jsShl"  
  unless (is32Bits $ vtype left) $ error "JavaScript SHL does not support floats"  
  thirtyOne <- D.i32c 31
  shiftCount <- D.and (vnode right) thirtyOne
  result <- D.safeSll (vnode left) shiftCount
  resultVar <- D.i32v "jsShlResult"
  D.assign result resultVar
  constVar <- D.i32v "jsShlConst"
  D.assign constVar $ vnode right
  val <- D.i32v "jsShlVal"
  D.assign val $ vnode left
  undef <- D.i1c 0
  return $ VNode undef result Signed

-- | https://es5.github.io/#x11.7.2
--
--  Let lnum be ToInt32(lval).
--
-- Let rnum be ToUint32(rval).
--
-- Let shiftCount be the result of masking out all but the least significant 5 bits of rnum,
-- that is, compute rnum & 0x1F.
--
-- Return the result of performing a sign-extending right shift of lnum by shiftCount bits.
-- The most significant bit is propagated. The result is a signed 32-bit integer.
--
jsShr :: VNode
      -> VNode
      -> D.Verif VNode
jsShr left right = do
  unless (numBits left == numBits right) $ error "Widths should match in jsShr"  
  unless (is32Bits $ vtype left) $ error "JavaScript SHL does not support floats"    
  thirtyOne <- D.i32c 31
  shiftCount <- D.and (vnode right) thirtyOne
  result <- D.safeSra (vnode left) shiftCount
  resultVar <- D.i32v "jsShrResult"
  D.assign result resultVar
  undef <- D.i1c 0
  return $ VNode undef result Signed

-- | https://es5.github.io/#x11.7.3
--
--  Let lnum be ToUint32(lval).
--
--  Let rnum be ToUint32(rval).
--
--  Let shiftCount be the result of masking out all but the least significant 5 bits of rnum,
-- that is, compute rnum & 0x1F.
--
--  Return the result of performing a zero-filling right shift of lnum by shiftCount bits.
-- Vacated bits are filled with zero. The result is an unsigned 32-bit integer.
--
jsUshr :: VNode
       -> VNode
       -> D.Verif VNode
jsUshr left right = do
  unless (numBits left == numBits right) $ error "Widths should match"  
  unless (is32Bits $ vtype left) $ error "JavaScript SHL does not support floats"    
  thirtyOne <- D.i32c 31
  shiftCount <- D.and (vnode right) thirtyOne
  result <- D.safeSrl (vnode left) shiftCount
  resultVar <- D.i32v "jsUshrResult"
  D.assign result resultVar            
  undef <- D.i1c 0
  return $ VNode undef result Signed

jsMul :: VNode
      -> VNode
      -> D.Verif VNode
jsMul node1 node2 = do
  unless (vtype node1 == vtype node2) $ error "Types should match in jsMul"
  let op = getOp node1 D.mul D.fpMul
  result <- op (vnode node1) (vnode node2)
  newDefinedNode result $ vtype node1

jsDiv :: VNode
      -> VNode
      -> D.Verif VNode
jsDiv node1 node2 = do
  unless (vtype node1 == vtype node2) $ error "Types should match in jsMul"
  let op = getOp node1 D.sdiv D.fpDiv
  result <- op (vnode node1) (vnode node2)
  newDefinedNode result $ vtype node1

jsRem :: VNode
      -> VNode
      -> D.Verif VNode
jsRem node1 node2 = do
  unless (vtype node1 == vtype node2) $ error "Types should match in jsMul"
  let op = getOp node1 D.srem D.fpRem
  result <- op (vnode node1) (vnode node2)
  newDefinedNode result $ vtype node1                                  

-- | https://es5.github.io/#x15.8.2.12
-- If no arguments are given, the result is +∞.
-- If any value is NaN, the result is NaN.
-- The comparison of values to determine the smallest value is done as in 11.8.5
-- except that +0 is considered to be larger than −0
jsMin :: VNode
      -> VNode
      -> D.Verif VNode
jsMin node1 node2 = do
  unless (vtype node1 == vtype node2) $ error "Types should match"
  -- Actually do the operation
  let op = getOp node1 D.smin D.fpMin 
  result <- op (vnode node1) (vnode node2)
  -- If its 32 bits, thats all, otherwise we have to handle nans
  if is32Bits $ vtype node1
  then newDefinedNode result $ vtype node1
  else do 
    -- If anything is Nan the result is Nan
    leftIsNan <- D.isNan $ vnode node1
    rightIsNan <- D.isNan $ vnode node2
    eitherIsNan <- D.or leftIsNan rightIsNan 
    _nan <- D.nan
    -- Return Nan or the result
    nanOrResult <- D.cond eitherIsNan _nan result
    newDefinedNode nanOrResult $ vtype node1  

-- | https://es5.github.io/#x15.8.2.11
-- If no arguments are given, the result is −∞.
-- If any value is NaN, the result is NaN.
-- The comparison of values to determine the largest value is done as in 11.8.5
-- except that +0 is considered to be larger than −0.
jsMax :: VNode
      -> VNode
      -> D.Verif VNode                    
jsMax node1 node2 = do
  unless (vtype node1 == vtype node2) $ error "Types should match: jsmax"
  let op = getOp node1 D.smax D.fpMax
  result <- op (vnode node1) (vnode node2)
  if is32Bits $ vtype node1
  then newDefinedNode result $ vtype node1
  else do 
    -- If anything is Nan the result is Nan 
    leftIsNan <- D.isNan $ vnode node1
    rightIsNan <- D.isNan $ vnode node2
    eitherIsNan <- D.or leftIsNan rightIsNan 
    _nan <- D.nan
    nanOrResult <- D.cond eitherIsNan _nan result
    newDefinedNode nanOrResult $ vtype node1    
                 
-- | https://es5.github.io/#x15.8.2.1
-- If x is NaN, the result is NaN.
-- If x is −0, the result is +0.
-- If x is −∞, the result is +∞.
jsAbs :: VNode
      -> D.Verif VNode 
jsAbs op = do
  if is32Bits $ vtype op
  then do
    _0 <- D.i32c 0
    _isNeg <- D.slt (vnode op) _0
    negOp <- D.neg (vnode op)
    result <- D.cond _isNeg negOp (vnode op)
    newDefinedNode result $ vtype op
  else do 
    _isNan <- D.isNan $ vnode op
    _nan <- D.nan
    result <- do
      _isNeg <- D.isNeg $ vnode op
      negOp <- D.fpNeg $ vnode op
      D.cond _isNeg negOp $ vnode op
    nanOrResult <- D.cond _isNan _nan result
    resVar <- D.doubv "abs_res"
    D.assign nanOrResult resVar
    beforeRes <- newDefinedNode nanOrResult $ vtype op
    resVarTwo <- fp "abs_after"
    vassign beforeRes resVarTwo
    return $ beforeRes

-- | https://es5.github.io/#x15.8.2.9
jsFloor :: VNode
        -> D.Verif VNode
jsFloor op =
  if is32Bits $ vtype op
  then return op
  else do 
    result <- D.fpFloor $ vnode op
    newDefinedNode result Double

-- | https://es5.github.io/#x15.8.2.9
jsCeil :: VNode
        -> D.Verif VNode
jsCeil op = do
  if is32Bits $ vtype op
  then return op
  else do 
    result <- D.fpCeil $ vnode op
    newDefinedNode result Double                 

-- | Have not found this one yet but we're guessing based on js
-- The mathematical function sign(x) yields 1 if x is positive and −1 if x is negative.
-- The sign function is not used in this standard for cases when x is zero.
jsSign :: VNode
       -> D.Verif VNode
jsSign op =
  if is32Bits $ vtype op
  then do
    _0 <- D.i32c 0
    _1 <- D.i32c 1
    _n1 <- D.i32c (-1)
    _isNeg <- D.slt (vnode op) _0
    result' <- D.cond _isNeg _n1 _1
    -- if it's zero, return zero
    _isZero <- D.iseq (vnode op) _0
    result <- D.cond _isZero _0 result'
    -- make a variable 
    newDefinedNode result $ vtype op 
  else do
    one <- D.double 1
    minusOne <- D.double (-1)
    _isPos <- D.isPos $ vnode op
    result' <- D.cond _isPos one minusOne
    -- if its pos zero return pos zero, neg zero return neg zero
    _isZero <- D.isZero $ vnode op
    _posZero <- D.fpzero False
    _negZero <- D.fpzero True
    correctZero <- D.cond _isPos _posZero _negZero
    result <- D.cond _isZero correctZero result'
    -- no result for nan
    _isNan <- D.isNan $ vnode op
    D.not _isNan >>= D.assert
    newDefinedNode result Double
