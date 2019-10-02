module DSL.Typed ( vassert
                 , vassign
                 , bool
                 , int32
                 , uint32
                 , num
                 , unum
                 , VNode
                 , intMax
                 , intMin
                 , uintMax
                 , uintMin
                 , cppEq
                 , cppAnd
                 , cppOr
                 , cppMin
                 , cppMax
                 , cppGt
                 , cppGte
                 , cppLt
                 , cppLte
                 , cppShiftLeft
                 , cppShiftRight
                 , D.runSolver
                 , D.evalVerif
                 ) where
import           Control.Monad.State.Strict (liftIO, unless)
import qualified DSL.DSL                    as D
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
-- Verbose, typed nodes
--

-- | Verbose verification node
data VNode = VNode { vundef :: D.Node
                   , vnode  :: D.Node
                   , vtype  :: Type
                   }

-- | Type of the node. For now we just have signed and unsigned i32s,
-- but eventually we will have more integer types
data Type = Unsigned
          | Signed
          | Double
          | Bool
          deriving (Eq, Ord, Show)

-- | Is the type a signed 32?
isSigned :: Type -> Bool
isSigned Signed = True
isSigned _      = False

-- | Is the type unsigned 32?
isUnsigned :: Type -> Bool
isUnsigned = not . isSigned

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

vassert :: VNode -> D.Verif ()
vassert = D.assert . vnode

vassign :: VNode -> VNode -> D.Verif ()
vassign n1 n2 = do
  unless (vtype n1 == vtype n2) $ error "Tried to assign different typed nodes"
  D.eq (vnode n1) (vnode n2) >>= D.assert
  D.eq (vundef n1) (vundef n2) >>= D.assert

--
-- Variables and constants
--

bool :: String -> D.Verif VNode
bool name = do
  var <- D.i1v name
  undef <- D.i1v $ name ++ "_undef"
  return $ VNode undef var Bool

int32 :: String -> D.Verif VNode
int32 name = do
  var <- D.i32v name
  undef <- D.i1v $ name ++ "_undef"
  return $ VNode undef var Signed

uint32 :: String -> D.Verif VNode
uint32 name = do
  var <- D.i32v name
  undef <- D.i1v $ name ++ "_undef"
  return $ VNode undef var Unsigned

unum :: Integer -> D.Verif VNode
unum val = do
  node <- D.i32c val
  newDefinedNode node Unsigned

num :: Integer -> D.Verif VNode
num val = do
  node <- D.i32c val
  newDefinedNode node Signed

intMax :: D.Verif VNode
intMax = num 2147483647

intMin :: D.Verif VNode
intMin = num (-2147483648)

uintMax :: D.Verif VNode
uintMax = unum 4294967295

uintMin :: D.Verif VNode
uintMin = unum 0

--
-- Operations
--

noopWrapper :: VNode
            -> VNode
            -> (D.Node -> D.Node -> D.Verif D.Node)
            -> D.Verif VNode
noopWrapper left right op = do
  result <- op (vnode left) (vnode right)
  let ty = if isUnsigned (vtype left) && isUnsigned (vtype right) then Unsigned else Signed
  newMaybeDefinedNode left right result ty

cppEq :: VNode
      -> VNode
      -> D.Verif VNode
cppEq left right = noopWrapper left right D.eq

cppOr :: VNode
      -> VNode
      -> D.Verif VNode
cppOr left right = noopWrapper left right D.or

cppAnd :: VNode
       -> VNode
       -> D.Verif VNode
cppAnd left right = noopWrapper left right D.and

cppMin :: VNode
       -> VNode
       -> D.Verif VNode
cppMin right left
  | isUnsigned (vtype right) && isUnsigned (vtype left) = noopWrapper left right D.umin
  | isSigned (vtype right) && isSigned (vtype left) = noopWrapper left right D.smin
  | otherwise = error "Compiler error: Can't use std:min on a signed and unsigned"

cppMax :: VNode
       -> VNode
       -> D.Verif VNode
cppMax right left
  | isUnsigned (vtype right) && isUnsigned (vtype left) = noopWrapper left right D.umax
  | isSigned (vtype right) && isSigned (vtype left) = noopWrapper left right D.smax
  | otherwise = error "Compiler error: Can't use std:max on a signed and unsigned"

cppCompareWrapper :: VNode
                  -> VNode
                  -> (D.Node -> D.Node -> D.Verif D.Node)
                  -> (D.Node -> D.Node -> D.Verif D.Node)
                  -> D.Verif VNode
cppCompareWrapper left right uCompare sCompare
 | isUnsigned (vtype left) || isUnsigned (vtype right) = do
     compare <- uCompare (vnode left) (vnode right)
     newMaybeDefinedNode left right compare Bool
 | otherwise = do
     compare <- sCompare (vnode left) (vnode right)
     newMaybeDefinedNode left right compare Bool

cppGt :: VNode -> VNode -> D.Verif VNode
cppGt left right = cppCompareWrapper left right D.ugt D.sgt

cppGte :: VNode -> VNode -> D.Verif VNode
cppGte left right = cppCompareWrapper left right D.ugte D.sgte

cppLt :: VNode -> VNode -> D.Verif VNode
cppLt left right = cppCompareWrapper left right D.ult D.slt

cppLte :: VNode -> VNode -> D.Verif VNode
cppLte left right = cppCompareWrapper left right D.ulte D.slte

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
cppShiftLeft :: VNode -> VNode -> D.Verif VNode
cppShiftLeft left right
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
      return $ VNode undef result Unsigned

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
      opUndef1 <- D.eq top32 zero >>= D.not
      opUndef2 <- D.ugt (vnode right) thirtyTwo
      opUndef <- D.or opUndef1 opUndef2
      parentsUndef <- D.or (vundef left) (vundef right)
      undef <- D.or opUndef parentsUndef

      result <- D.safeSll (vnode left) (vnode right)
      return $ VNode undef result Signed

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
cppShiftRight :: VNode -> VNode -> D.Verif VNode
cppShiftRight left right
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
