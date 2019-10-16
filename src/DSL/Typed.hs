{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
module DSL.Typed ( vassert
                 , vassign
                 , assertUndef
                 , assertDef
                 , newInputVar
                 , newResultVar
                 -- * Query
                 , is32Bits
                 , is64Bits
                 , is16Bits
                 , is8Bits
                 , isDouble
                 -- * Constants and variables
                 , true
                 , false
                 , bool
                 , int32
                 , uint32
                 , int64
                 , uint64
                 , int16
                 , uint16
                 , fp
                 , num
                 , unum
                 , num64
                 , unum64
                 , num16
                 , unum16
                 , fpnum
                 , named
                 -- * Types 
                 , VNode(..)
                 , Type(..)
                 , vtype
                 -- * Min/Max constants 
                 , intMax
                 , intMin
                 , uintMax
                 , uintMin
                   -- * Floating point specific 
                 , posInf
                 , negInf
                 , posZero
                 , negZero
                 , nan
                 , isInf
                 , isNan
                 , isNeg
                 , isPos
                 , isZero
                 , getFpExponent
                   -- * Js operations: what we are using to verify
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
                 , jsAbs
                 , jsMin
                 , jsMax
                 , jsSign
                 , jsFloor
                 , jsCeil
                   -- * Cpp operations: building blocks for what we are verifying
                 , cppNeg
                 , cppNot
                 , cppEq
                 , cppAnd
                 , cppAdd
                 , cppSub
                 , cppMul
                 , cppOr
                 , cppMin
                 , cppMax
                 , cppGt
                 , cppGte
                 , cppLt
                 , cppLte
                 , cppShiftLeft
                 , cppShiftRight
                 , cppCond
                 , cppCast
                 , cppAbs
                 , cppXor
                 -- * Running the solver and getting the model
                 , D.runSolver
                 , D.evalVerif
                 , D.SMTResult(..)
                 , D.Verif
                 , D.isSat
                 , D.isUnsat
                 ) where
import           Control.Monad.State.Strict (liftIO, unless, when)
import qualified DSL.DSL                    as D
import qualified DSL.Z3Wrapper              as D
import           Prelude                    hiding (compare)

{-|

Typed wrappers around C++ operations.
Why? To avoid having to figure this kind of thing out every time:

int x;
uint y;
if (x < y) <--- "Do I use a signed or unsigned SMT comparison?"

We also want to propagate undefined behavior. Boolector operations have no
notion of undef behavior, so we have to keep track of whether or not it has
happened according to the C++ standard.

Therefore, this file is gonna do three things:
- Type inference on nodes
- C++ operations polymorphically over all possible types
- Undefined behavior propagation

-}

--
-- Helper macros
--

#define DEFINEUNIOPCLASS(ClassName, opName) \
class ClassName n0 where { \
  opName :: n0 -> D.Verif VNode \
};\
instance ClassName (D.Verif VNode) where { \
  opName m0 = m0 >>= \n0 -> opName n0 \
};

#define DEFINEBINOPCLASS(ClassName, opName) \
class ClassName n0 n1 where { \
  opName :: n0 -> n1 -> D.Verif VNode \
};\
instance ClassName (D.Verif VNode) VNode where { \
  opName m0 n1 = m0 >>= \n0 -> opName n0 n1 \
};\
instance ClassName (D.Verif VNode) (D.Verif VNode) where { \
  opName m0 m1 = m0 >>= \n0 -> m1 >>= \n1 -> opName n0 n1 \
};\
instance ClassName VNode (D.Verif VNode) where { \
  opName n0 m1 = m1 >>= \n1 -> opName n0 n1 \
};


#define DEFINETEROPCLASS(ClassName, opName) \
class ClassName n0 n1 n2 where { \
  opName :: n0 -> n1 -> n2 -> D.Verif VNode \
};\
instance ClassName (D.Verif VNode) VNode VNode where { \
  opName m0 n1 n2 = m0 >>= \n0 -> opName n0 n1 n2 \
};\
instance ClassName (D.Verif VNode) (D.Verif VNode) VNode where { \
  opName m0 m1 n2 = m0 >>= \n0 -> m1 >>= \n1 -> opName n0 n1 n2 \
};\
instance ClassName (D.Verif VNode) VNode (D.Verif VNode) where { \
  opName m0 n1 m2 = m0 >>= \n0 -> m2 >>= \n2 -> opName n0 n1 n2 \
};\
instance ClassName (D.Verif VNode) (D.Verif VNode) (D.Verif VNode) where { \
  opName m0 m1 m2 = m0 >>= \n0 -> m1 >>= \n1 -> m2 >>= \n2 -> opName n0 n1 n2 \
};\
instance ClassName VNode (D.Verif VNode) VNode where { \
  opName n0 m1 n2 = m1 >>= \n1 -> opName n0 n1 n2 \
};\
instance ClassName VNode (D.Verif VNode) (D.Verif VNode) where { \
  opName n0 m1 m2 = m1 >>= \n1 -> m2 >>= \n2 -> opName n0 n1 n2 \
};\
instance ClassName VNode VNode (D.Verif VNode) where {\
  opName n0 n1 m2 = m2 >>= \n2 -> opName n0 n1 n2 \
};



--
-- Verbose, typed nodes
--

-- | Verbose verification node
data VNode = VNode { vundef :: D.Node
                   , vnode  :: D.Node
                   , vtype  :: Type
                   }
           deriving (Eq, Ord, Show)

-- | Type of the node. For now we just have signed and unsigned i32s,
-- but eventually we will have more integer types
data Type = Unsigned
          | Signed
          | Unsigned64
          | Signed64
          | Unsigned16
          | Signed16
          | Unsigned8
          | Signed8
          | Double
          | Bool
          deriving (Eq, Ord, Show)

-- | Is the type signed?
isSigned :: Type -> Bool
isSigned Signed   = True
isSigned Signed64 = True
isSigned Signed16 = True
isSigned Signed8  = True
isSigned _        = False

-- | Is the unsigned?
isUnsigned :: Type -> Bool
isUnsigned = not . isSigned

isDouble :: Type -> Bool
isDouble Double = True
isDouble _      = False

isBool :: Type -> Bool
isBool Bool = True
isBool _ = False 
                  
numBits :: VNode -> Int
numBits = numBits' . vtype

numBits' :: Type -> Int
numBits' Bool = 1
numBits' Double = 1000 -- Code for a double
numBits' ty
    | is32Bits ty = 32
    | is64Bits ty = 64
    | is16Bits ty = 16
    | is8Bits ty  = 8
    | otherwise   = error "Not supported type"

is32Bits :: Type -> Bool
is32Bits Signed   = True
is32Bits Unsigned = True
is32Bits _        = False

is64Bits :: Type -> Bool
is64Bits Signed64   = True
is64Bits Unsigned64 = True
is64Bits _          = False

is16Bits :: Type -> Bool
is16Bits Signed16   = True
is16Bits Unsigned16 = True
is16Bits _          = False

is8Bits :: Type -> Bool
is8Bits Signed8   = True
is8Bits Unsigned8 = True
is8Bits _         = False                      

newInputVar :: Type
            -> String
            -> D.Verif VNode
newInputVar ty name = do
  var <- case ty of
           Bool       -> D.i1v name
           Signed     -> D.i32v name
           Unsigned   -> D.i32v name
           Signed64   -> D.i64v name
           Unsigned64 -> D.i64v name
           Signed16   -> D.i16v name
           Unsigned16 -> D.i16v name
           Signed8    -> D.i8v name
           Unsigned8  -> D.i8v name 
           Double     -> D.doubv name 
           _          -> error "Not yet supported"
  undef <- D.i1c 0
  return $ VNode undef var ty

newResultVar :: Type
             -> String
             -> D.Verif VNode
newResultVar Bool       = bool
newResultVar Signed     = int32
newResultVar Unsigned   = uint32
newResultVar Signed64   = int64
newResultVar Unsigned64 = uint64
newResultVar Signed16   = int16
newResultVar Unsigned16 = uint16
newResultVar Signed8    = int8
newResultVar Unsigned8  = uint8
newResultVar Double     = fp
newResultVar _          = error "No more"


-- | Make a new node whose undef flag may be set, based on the parents of the node
newMaybeDefinedNode :: VNode
                    -> VNode
                    -> D.Node
                    -> Type
                    -> D.Verif VNode
newMaybeDefinedNode parent1 parent2 node ty = do
  childUndef <- D.or (vundef parent1) (vundef parent2)
  return $ VNode childUndef node ty

-- | Make a new node whose undef flag is not set
newDefinedNode :: D.Node -> Type -> D.Verif VNode
newDefinedNode node ty = do
  undefBit <- D.i1c 0
  return $ VNode undefBit node ty

--

-- | Name an intermediate node (action). Useful for debugging.
named :: String -> D.Verif VNode -> D.Verif VNode
named name act = do
  n <- act
  D.named name $ vnode n
  return n


vassert :: VNode -> D.Verif ()
vassert = D.assert . vnode

vassign :: VNode -> VNode -> D.Verif ()
vassign n1 n2 = do
  unless (vtype n1 == vtype n2) $ error $ unwords ["Tried to assign different typed nodes"
                                                  , show $ vtype n1
                                                  , show $ vtype n2
                                                  ]
  when (not $ isDouble $ vtype n1) $ do
    sort1 <- D.bvSize $ vnode n1
    sort2 <- D.bvSize $ vnode n2
    unless (sort1 == sort2) $ error $ unwords [ "Mismatched widths to assignment:"
                                              , show sort1
                                              , "and"
                                              , show sort2
                                              ]
  D.eq (vnode n1) (vnode n2) >>= D.assert
  D.eq (vundef n1) (vundef n2) >>= D.assert

assertUndef :: VNode -> D.Verif ()
assertUndef node = do
  one <- D.i1c 1
  D.eq (vundef node) one >>= D.assert

assertDef :: VNode -> D.Verif ()
assertDef node = do
  zero <- D.i1c 0
  D.eq (vundef node) zero >>= D.assert

--
-- Variables and constants
--

makeVar :: String -> (String -> D.Verif D.Node) -> Type -> D.Verif VNode
makeVar name varMaker ty = do
  var <- varMaker name
  undef <- D.i1v $ name ++ "_undef"
  return $ VNode undef var ty

bool :: String -> D.Verif VNode
bool name = makeVar name D.i1v Bool

int32 :: String -> D.Verif VNode
int32 name = makeVar name D.i32v Signed

uint32 :: String -> D.Verif VNode
uint32 name = makeVar name D.i32v Unsigned

int64 :: String -> D.Verif VNode
int64 name = makeVar name D.i64v Signed64

uint64 :: String -> D.Verif VNode
uint64 name = makeVar name D.i64v Unsigned64

int16 :: String -> D.Verif VNode
int16 name = makeVar name D.i16v Signed16

uint16 :: String -> D.Verif VNode
uint16 name = makeVar name D.i16v Unsigned16

int8 :: String -> D.Verif VNode
int8 name = makeVar name D.i16v Signed8

uint8 :: String -> D.Verif VNode
uint8 name = makeVar name D.i16v Unsigned8

fp :: String -> D.Verif VNode
fp name = makeVar name D.doubv Double 
              
makeNum :: Integer -> (Integer -> D.Verif D.Node) -> Type -> D.Verif VNode
makeNum val numMaker ty = do
  node <- numMaker val
  newDefinedNode node ty

false :: D.Verif VNode
false = makeNum 0 D.i1c Bool

true :: D.Verif VNode
true = makeNum 1 D.i1c Bool

unum :: Integer -> D.Verif VNode
unum val = makeNum val D.i32c Unsigned

num :: Integer -> D.Verif VNode
num val = makeNum val D.i32c Signed

unum64 :: Integer -> D.Verif VNode
unum64 val = makeNum val D.i64c Unsigned64

num64 :: Integer -> D.Verif VNode
num64 val = makeNum val D.i64c Signed64

unum16 :: Integer -> D.Verif VNode
unum16 val = makeNum val D.i16c Unsigned16

num16 :: Integer -> D.Verif VNode
num16 val = makeNum val D.i16c Signed16

fpnum :: Double -> D.Verif VNode
fpnum val = do
  node <- D.double val 
  newDefinedNode node Double  
            
--
--
--

intMax :: D.Verif VNode
intMax = num 2147483647

intMin :: D.Verif VNode
intMin = num (-2147483648)

uintMax :: D.Verif VNode
uintMax = unum 4294967295

uintMin :: D.Verif VNode
uintMin = unum 0

--
-- Floating point
--

posInf :: D.Verif VNode
posInf = do
  inf <- D.inf True
  newDefinedNode inf Double 

negInf :: D.Verif VNode
negInf = do
  inf <- D.inf False 
  newDefinedNode inf Double   

posZero :: D.Verif VNode
posZero = do
  z <- D.fpzero True
  newDefinedNode z Double

negZero :: D.Verif VNode
negZero = do
  z <- D.fpzero False
  newDefinedNode z Double 

nan :: D.Verif VNode                 
nan = do
  nan <- D.nan
  newDefinedNode nan Double

isInf :: VNode -> D.Verif VNode                 
isInf n = do
  c <- D.isInf $ vnode n
  newMaybeDefinedNode n n c Bool

isNan :: VNode -> D.Verif VNode                      
isNan n = do
  c <- D.isNan $ vnode n
  newMaybeDefinedNode n n c Bool

isNeg :: VNode -> D.Verif VNode                      
isNeg n = do
  c <- D.isNeg $ vnode n
  newMaybeDefinedNode n n c Bool
        
isPos :: VNode -> D.Verif VNode
isPos n = do
  c <- D.isPos $ vnode n
  newMaybeDefinedNode n n c Bool

isZero :: VNode -> D.Verif VNode                       
isZero n = do
  c <- D.isZero $ vnode n
  newMaybeDefinedNode n n c Bool
          
--
-- JavaScript operations
--

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
    nan <- D.nan
    -- Return Nan or the result
    nanOrResult <- D.cond eitherIsNan nan result
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
    nan <- D.nan
    nanOrResult <- D.cond eitherIsNan nan result
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
    isNeg <- D.slt (vnode op) _0
    negOp <- D.neg (vnode op)
    result <- D.cond isNeg negOp (vnode op)
    newDefinedNode result $ vtype op
  else do 
    isNan <- D.isNan $ vnode op
    nan <- D.nan
    result <- do
      isNeg <- D.isNeg $ vnode op
      negOp <- D.fpNeg $ vnode op
      D.cond isNeg negOp $ vnode op
    nanOrResult <- D.cond isNan nan result
    resultVar <- D.doubv "jsAbsResult"
    D.assign nanOrResult resultVar  
    newDefinedNode nanOrResult $ vtype op

-- | https://es5.github.io/#x15.8.2.9
jsFloor :: VNode
        -> D.Verif VNode
jsFloor op =
  if is32Bits $ vtype op
  then return op
  else do 
    result <- D.fpFloor $ vnode op
    resultVar <- D.doubv "jsFloorResult"
    D.assign result resultVar
    newDefinedNode result Double

-- | https://es5.github.io/#x15.8.2.9
jsCeil :: VNode
        -> D.Verif VNode
jsCeil op = 
  if is32Bits $ vtype op
  then return op
  else do 
    result <- D.fpCeil $ vnode op
    resultVar <- D.doubv "jsCailResult"
    D.assign result resultVar
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
    isNeg <- D.slt (vnode op) _0
    result <- D.cond isNeg _n1 _1
    newDefinedNode result $ vtype op 
  else do
    one <- D.double 1
    minusOne <- D.double (-1)
    isPos <- D.isPos $ vnode op
    result <- D.cond isPos one minusOne
    resultVar <- D.doubv "jsSign"
    D.assign result resultVar
    newDefinedNode result Double
  
--
-- C++ Operations
--

noopWrapper :: VNode
            -> VNode
            -> (D.Node -> D.Node -> D.Verif D.Node)
            -> Maybe (Bool -> D.Node -> D.Node -> D.Verif D.Node)
            -> String 
            -> D.Verif VNode
noopWrapper left right op overflowOp opName = do
  unless (numBits left == numBits right) $
    error $ unwords ["Mismatched bits to operation"
                    , opName 
                    , ":"
                    , show $ numBits left
                    , "and"
                    , show $ numBits right
                    ]
  parentsUndef <- D.or (vundef left) (vundef right)
  canOverflow <- case overflowOp of
                   Nothing  -> return parentsUndef
                   Just oop -> do
                     ct <- D.getNextCt
                     flow <- oop (isSigned $ vtype left) (vnode left) (vnode right)
                     D.or parentsUndef flow 
  result <- op (vnode left) (vnode right)
  let (s, u) = case numBits left of
                 1    -> (Bool, Bool)
                 8    -> (Signed8, Unsigned8)
                 16   -> (Signed16, Unsigned16)
                 32   -> (Signed, Unsigned)
                 64   -> (Signed64, Unsigned64)
                 1000 -> (Double, Double)
                 e    -> error $ unwords [ "Unsupported type for operation"
                                         , show e
                                         ]
  let ty = if isUnsigned (vtype left) && isUnsigned (vtype right) then u else s
  return $ VNode canOverflow result ty 

DEFINEUNIOPCLASS(CppNeg, cppNeg)
instance CppNeg VNode where
  cppNeg node = do
    let op = if isDouble $ vtype node then D.fpNeg else D.neg
    result <- op (vnode node)
    return $ VNode (vundef node) result (vtype node)

DEFINEUNIOPCLASS(CppNot, cppNot)
instance CppNot VNode where
  cppNot node = do
    when (isDouble $ vtype node) $ error "Cannot bitwise negate double"
    result <- D.not (vnode node)
    return $ VNode (vundef node) result (vtype node)

DEFINEBINOPCLASS(CppOr, cppOr)
instance CppOr VNode VNode where
  cppOr left right
    | (isDouble $ vtype left) || (isDouble $ vtype right) = error "No bitwise or for doubles"
    | otherwise = noopWrapper left right D.or Nothing "or"

DEFINEBINOPCLASS(CppAnd, cppAnd)
instance CppAnd VNode VNode where
  cppAnd left right
    | (isDouble $ vtype left) || (isDouble $ vtype right) = error "No bitwise and for doubles"
    | otherwise = noopWrapper left right D.and Nothing "and"

DEFINEBINOPCLASS(CppSub, cppSub)
instance CppSub VNode VNode where
  cppSub left right
    | isDouble (vtype left) || isDouble (vtype right) =
        noopWrapper left right D.fpSub Nothing "sub"
    | otherwise = noopWrapper left right D.sub (Just D.subUndef) "sub"

DEFINEBINOPCLASS(CppMul, cppMul)
instance CppMul VNode VNode where
  cppMul left right
    | isDouble (vtype left) || isDouble (vtype right) =
        noopWrapper left right D.fpMul Nothing "mul"
    | otherwise =
        noopWrapper left right D.mul (Just D.mulUndef) "mul"
                  
DEFINEBINOPCLASS(CppAdd, cppAdd)
instance CppAdd VNode VNode where
  cppAdd left right
    | isDouble (vtype left) || isDouble (vtype right) =
        noopWrapper left right D.fpAdd Nothing "add"
    | otherwise =
        noopWrapper left right D.add (Just D.addUndef) "add"

DEFINEBINOPCLASS(CppMin, cppMin)
instance CppMin VNode VNode where
  cppMin right left
    | isDouble (vtype right) || isDouble (vtype left) =
        noopWrapper left right D.fpMin Nothing "min"
    | isUnsigned (vtype right) && isUnsigned (vtype left)
        = noopWrapper left right D.umin Nothing "min"
    | isSigned (vtype right) && isSigned (vtype left) =
        noopWrapper left right D.smin Nothing "min"
    | otherwise = error "Compiler error: Can't use std:min on a signed and unsigned"

DEFINEBINOPCLASS(CppMax, cppMax)
instance CppMax VNode VNode where
  cppMax right left
    | isDouble (vtype right) || isDouble (vtype left) =
        noopWrapper left right D.fpMax Nothing "max"
    | isUnsigned (vtype right) && isUnsigned (vtype left) =
        noopWrapper left right D.umax Nothing "max"
    | isSigned (vtype right) && isSigned (vtype left) =
        noopWrapper left right D.smax Nothing "max" 
    | otherwise = error "Compiler error: Can't use std:max on a signed and unsigned"

cppCompareWrapper :: VNode
                  -> VNode
                  -> (D.Node -> D.Node -> D.Verif D.Node)
                  -> (D.Node -> D.Node -> D.Verif D.Node) 
                  -> (D.Node -> D.Node -> D.Verif D.Node)                    
                  -> D.Verif VNode
cppCompareWrapper left right uCompare sCompare fCompare 
 | isDouble (vtype left) || isDouble (vtype right) = do
     unless (vtype left == vtype right) $ error "Expected two doubles as argumnets"
     compare <- fCompare (vnode left) (vnode right)
     newMaybeDefinedNode left right compare Bool
 | isUnsigned (vtype left) || isUnsigned (vtype right) = do
     compare <- uCompare (vnode left) (vnode right)
     newMaybeDefinedNode left right compare Bool
 | otherwise = do
     compare <- sCompare (vnode left) (vnode right)
     newMaybeDefinedNode left right compare Bool

DEFINEBINOPCLASS(CppEq, cppEq)
instance CppEq VNode VNode where
  cppEq left right = cppCompareWrapper left right D.iseq D.iseq D.fpEq
                         
DEFINEBINOPCLASS(CppGt, cppGt)
instance CppGt VNode VNode where
  cppGt left right = cppCompareWrapper left right D.ugt D.sgt D.fpGt

DEFINEBINOPCLASS(CppGte, cppGte)
instance CppGte VNode VNode where
  cppGte left right = cppCompareWrapper left right D.ugte D.sgte D.fpGte

DEFINEBINOPCLASS(CppLt, cppLt)
instance CppLt VNode VNode where
  cppLt left right = cppCompareWrapper left right D.ult D.slt D.fpLt

DEFINEBINOPCLASS(CppLte, cppLte)
instance CppLte VNode VNode where
  cppLte left right = cppCompareWrapper left right D.ulte D.slte D.fpLte

-- | C++ left shift operator. We are consulting CPP instead of Clang reference
-- because we need to know what the CPP compiler will actually generate.
--
-- https://en.cppreference.com/w/cpp/language/operator_arithmetic#Bitwise_shift_operators
--
-- For unsigned a, the value of a << b is the value of a * 2^b, reduced modulo 2^N
-- where N is the number of bits in the return type (that is, bitwise left shift
-- is performed and the bits that get shifted out of the destination type are discarded).
--
-- C++ 14:
-- For signed and non-negative a, the value of a << b is a * 2^b
-- if it is representable in the return type, otherwise the behavior is undefined.
--
-- Since C++ 14:
-- For signed and non-negative a, if a * 2^b is representable in the unsigned version of
-- the return type, then that value, converted to signed, is the value of a << b
-- (this makes it legal to create INT_MIN as 1<<31); otherwise the behavior is undefined.
-- TODO: ARE WE DOING THIS RIGHT?
--
-- For negative a, the behavior of a << b is undefined.
--
-- Via Clang interpreter typeid function:
--
-- int = int << anything
-- uint = uint << anything
--
-- Clang turns 1 << -1 into undef
--
DEFINEBINOPCLASS(CppShiftLeft, cppShiftLeft)
instance CppShiftLeft VNode VNode where
  cppShiftLeft left right
    | not (is32Bits $ vtype left) || not (is32Bits $ vtype right) =
        error "Only support 32 bit SHL"
    | isUnsigned (vtype left) = do
        parentsUndef <- D.or (vundef left) (vundef right)
        -- If the right is signed and negative, undefined behavior
        undef <- if isSigned (vtype right)
                then do
                  zero <- D.i32c 0
                  opUndef <- D.slt (vnode right) zero
                  D.or parentsUndef opUndef
                else return parentsUndef

        result <- D.safeSll (vnode left) (vnode right)
        return $ VNode undef result $ vtype left

    | otherwise = do

        -- Do the operation in 64-bits and see if any of the left bits are set.
        -- If so, the operation has undefined behavior because some bit was
        -- shifted off the end of the 32-bit variable
        left64 <- D.uext (vnode left) 32
        right64 <- D.uext (vnode right) 32
        result64 <- D.safeSll left64 right64
        top32 <- D.slice result64 63 32
        zero <- D.i32c 0

        -- If the right is greater than 32, it is definitely undef
        -- This will also catch trying to shift by a negative
        thirtyTwo <- D.i32c 32

        -- Is it undef?
        opUndef1 <- D.iseq top32 zero >>= D.not
        opUndef2 <- D.ugt (vnode right) thirtyTwo
        opUndef <- D.or opUndef1 opUndef2
        parentsUndef <- D.or (vundef left) (vundef right)
        undef <- D.or opUndef parentsUndef

        result <- D.safeSll (vnode left) (vnode right)
        return $ VNode undef result $ vtype left

-- | C++ right shift operator. We are consulating CPP instead of Clang reference
-- because we need to know what the CPP compiler does *before* generating IR.
-- For example, will the compiler generate ashr or lshr?
--
-- https://en.cppreference.com/w/cpp/language/operator_arithmetic
--
-- For unsigned a and for signed and non-negative a, the value of a >> b is the integer
-- part of a/2b
--
-- For negative a, the value of a >> b is implementation-defined
-- (in most implementations, this performs arithmetic right shift,
-- so that the result remains negative).
--
-- The value of a >> b is a/2b, rounded down (in other words, right shift on
-- signed a is arithmetic right shift).
--
-- Via Clang interpreter typeid function:
--
-- int = int >> anything
-- uint = uint >> anything
--
-- We are going to assume that Clang implements GCC's implementation-defined
-- behavior, since it appears that Clang does not actually document their impl-defined
-- behavior anywhere. GCC says:
--
-- Signed ‘>>’ acts on negative numbers by sign extension.
-- (https://gcc.gnu.org/onlinedocs/gcc/Integers-implementation.html#Integers-implementation)
--
-- Shifting by a negative is undefined behavior:
-- 5 >> -8 compiles at Clang -O3 to undef
--
DEFINEBINOPCLASS(CppShiftRight, cppShiftRight)
instance CppShiftRight VNode VNode where
  cppShiftRight left right
    | not (is32Bits $ vtype left) || not (is32Bits $ vtype right) =
        error "Only support 32 bit SHL"                
    | isUnsigned (vtype left) = do
        undef <- makeUndef
        result <- D.safeSrl (vnode left) (vnode right)
        return $ VNode undef result Unsigned
    | otherwise = do
        undef <- makeUndef
        result <- D.safeSra (vnode left) (vnode right)
        return $ VNode undef result Signed
    where
      makeUndef = do
        zero <- D.i32c 0
        opUndef <- D.slt (vnode right) zero
        parentsUndef <- D.or (vundef left) (vundef right)
        D.or opUndef parentsUndef

DEFINETEROPCLASS(CppCon, cppCond)
instance CppCon VNode VNode VNode where
  cppCond cond true false = do
    unless (vtype cond == Bool) $ error "Conditional must be a boolean"
    unless (vtype true == vtype false) $ error "Must have both branches of cond be same type"
    result <- D.cond (vnode cond) (vnode true) (vnode false)
    undef <- D.or (vundef cond) (vundef true) >>= D.or (vundef false)
    return $ VNode undef result $ vtype true

class CppCast n0 where
  cppCast :: n0 -> Type -> D.Verif VNode

instance CppCast (D.Verif VNode) where
  cppCast m0 ty = m0 >>= \n0 -> cppCast n0 ty

instance CppCast VNode where
  cppCast node toTy
    | isDouble fromTy = case toTy of
                          Signed     -> do
                            result <- D.castFp (vnode node) 32
                            return $ VNode (vundef node) result Signed
                          _          -> error "We only suppor Double to int32 casts rn"
    | is16Bits fromTy = case toTy of
                          Unsigned16 -> return $ VNode (vundef node) (vnode node) Unsigned16
                          Signed16   -> return $ VNode (vundef node) (vnode node) Signed16
                          Unsigned64 -> do
                            result <- D.uext (vnode node) 48
                            return $ VNode (vundef node) result Unsigned64
                          Signed64   -> do
                            result <- D.sext (vnode node) 48
                            return $ VNode (vundef node) result Signed64
                          Unsigned   -> do
                            result <- D.uext (vnode node) 16
                            return $ VNode (vundef node) result Unsigned
                          Signed     -> do
                            result <- D.sext (vnode node) 16
                            return $ VNode (vundef node) result Signed
                          Unsigned8  -> do
                            result <- D.slice (vnode node) 7 0
                            return $ VNode (vundef node) result Unsigned8
                          Signed8    -> do
                            result <- D.slice (vnode node) 7 0
                            return $ VNode (vundef node) result Signed8
                          _          -> error "Illegal cast types"
    | is8Bits fromTy = case toTy of
                          Unsigned8  -> return $ VNode (vundef node) (vnode node) Unsigned8
                          Signed8    -> return $ VNode (vundef node) (vnode node) Signed8
                          Unsigned64 -> do
                            result <- D.uext (vnode node) 56
                            return $ VNode (vundef node) result Unsigned64
                          Signed64   -> do
                            result <- D.sext (vnode node) 56
                            return $ VNode (vundef node) result Signed64
                          Unsigned   -> do
                            result <- D.uext (vnode node) 24
                            return $ VNode (vundef node) result Unsigned
                          Signed     -> do
                            result <- D.sext (vnode node) 24
                            return $ VNode (vundef node) result Signed
                          Unsigned16  -> do
                            result <- D.uext (vnode node) 8
                            return $ VNode (vundef node) result Unsigned16
                          Signed16    -> do
                            result <- D.sext (vnode node) 8
                            return $ VNode (vundef node) result Signed16
                          _          -> error "Illegal cast types"
    | is32Bits fromTy = case toTy of
                          Unsigned   -> return $ VNode (vundef node) (vnode node) Unsigned
                          Signed     -> return $ VNode (vundef node) (vnode node) Signed
                          Unsigned64 -> do
                            result <- D.uext (vnode node) 32
                            return $ VNode (vundef node) result Unsigned64
                          Signed64   -> do
                            result <- D.sext (vnode node) 32
                            return $ VNode (vundef node) result Signed64
                          Double     -> do
                            exted <- D.uext (vnode node) 32
                            result <- D.castBv exted
                            return $ VNode (vundef node) result Double
                          _          -> error "Illegal cast types"
    | is64Bits fromTy = case toTy of
                          Unsigned   -> do
                            result <- D.slice (vnode node) 31 0
                            return $ VNode (vundef node) result Unsigned
                          Signed     -> do
                            result <- D.slice (vnode node) 31 0
                            return $ VNode (vundef node) result Signed
                          Unsigned64 -> return $ VNode (vundef node) (vnode node) Unsigned64
                          Signed64   -> return $ VNode (vundef node) (vnode node) Signed64
                          _          -> error "Illegal cast types"
    | otherwise = error "Illegal cast types"
    where fromTy = vtype node

cppAbs :: VNode -> D.Verif VNode                    
cppAbs node = if isUnsigned $ vtype node
              then return node
              else do
                zero <- case vtype node of
                          Signed -> D.i32c 0
                          Double -> D.fpzero False 
                          _      -> error "Do not support all types for cpp abs"
                let (op, cmp) = case vtype node of
                           Signed -> (D.neg, D.slt)
                           Double -> (D.fpNeg, D.fpLt)
                           _      -> error "Do not support all types for cpp abs"
                cond <- cmp (vnode node) zero
                negated <- op (vnode node)
                result <- D.cond cond negated $ vnode node
                return $ VNode (vundef node) result $ vtype node

cppXor :: VNode -> VNode -> D.Verif VNode
cppXor n1 n2 = do
  unless (isBool (vtype n1) && isBool (vtype n2)) $ error "Only support xor bools"
  result <- D.xor (vnode n1) (vnode n2)
  undef <- D.or (vundef n1) (vundef n2)
  return $ VNode undef result Bool

getFpExponent :: VNode -> D.Verif VNode
getFpExponent node = do
  unless (isDouble $ vtype node) $ error "Cannot get exponent of non-double"
  bv <- D.ieeeBv $ vnode node
  -- get the exponent from the bitvector
  exp <- D.slice bv 62 52 >>= D.exponent
  -- get the significand
  sig <- D.slice bv 51 0
  -- if the significand is zero, the max exponent is just the exponent.
  -- otherwise, it is the exponent + 1 
  sigZero <- D.significandConst 0
  sigIsZero <- D.iseq sig sigZero
  expPlusOne <- D.exponentConst 1 >>= D.add exp 
  result <- D.cond sigIsZero exp expPlusOne
  castResult <- D.uext result 5
  return $ VNode (vundef node) castResult Unsigned16
