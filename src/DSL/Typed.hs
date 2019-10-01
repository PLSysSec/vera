module DSL.Typed where
import qualified DSL.DSL as D

{-|

Typed wrappers around C++ operations.
Why? To avoid having to figure this kind of thing out every time:

int x;
uint y;
if (x < y) <--- "Do I use a signed or unsigned SMT comparison?"

We also want to propagate undefined behavior. Boolector operations have no
notion of undef behavior, so we have to keep track of whether or not it has
happened according to the C++ standard.

-}

define :: D.Type
       -> (D.VNode -> D.VNode -> D.VNode)
       -> D.VNode
define = undefined


cppCompareWrapper :: D.VNode
                  -> D.VNode
                  -> (D.Node -> D.Node -> D.Verif D.Node)
                  -> (D.Node -> D.Node -> D.Verif D.Node)
                  -> D.Verif D.VNode
cppCompareWrapper left right uCompare sCompare
 | D.isUnsigned (D.vtype left) || D.isUnsigned (D.vtype right) = do
     compare <- uCompare (D.vnode left) (D.vnode right)
     D.newMaybeDefinedNode left right compare D.Bool
 | otherwise = do
     compare <- sCompare (D.vnode left) (D.vnode right)
     D.newMaybeDefinedNode left right compare D.Bool

cppGt :: D.VNode -> D.VNode -> D.Verif D.VNode
cppGt left right = cppCompareWrapper left right D.ugt D.sgt

cppGte :: D.VNode -> D.VNode -> D.Verif D.VNode
cppGte left right = cppCompareWrapper left right D.ugte D.sgte

cppLt :: D.VNode -> D.VNode -> D.Verif D.VNode
cppLt left right = cppCompareWrapper left right D.ult D.slt

cppLte :: D.VNode -> D.VNode -> D.Verif D.VNode
cppLte left right = cppCompareWrapper left right D.ulte D.slte

-- | C++ left shift operator. We are consulting CPP instead of Clang reference
-- because we need to know what the CPP compiler will actually generate.
--
-- https://en.cppreference.com/w/cpp/language/operator_arithmetic#Bitwise_shift_operators
--
-- For unsigned a, the value of a << b is the value of a * 2b, reduced modulo 2N
-- where N is the number of bits in the return type (that is, bitwise left shift
-- is performed and the bits that get shifted out of the destination type are discarded).
--
-- C++ 14:
-- For signed and non-negative a, the value of a << b is a * 2b
-- if it is representable in the return type, otherwise the behavior is undefined.
--
-- Since C++ 14:
-- For signed and non-negative a, if a * 2b is representable in the unsigned version of
-- the return type, then that value, converted to signed, is the value of a << b
-- (this makes it legal to create INT_MIN as 1<<31); otherwise the behavior is undefined.
--
-- For negative a, the behavior of a << b is undefined.
--
-- Via Clang interpreter typeid function:
--
-- int = int << anything
-- uint = uint << anything
cppShiftLeft :: D.VNode -> D.VNode -> D.Verif D.VNode
cppShiftLeft left right
  | D.isUnsigned (D.vtype left) = do
      result <- D.safeSll (D.vnode left) (D.vnode right)
      D.newMaybeDefinedNode left right result D.Unsigned
  | otherwise = do
      -- Do the operation in 32- and 64-bits to check for overflow
      left64 <- D.uext (D.vnode left) 32
      right64 <- D.uext (D.vnode right) 32
      result64 <- D.safeSll left64 right64 >>= \r -> D.slice r 32 0
      result32 <- D.safeSll (D.vnode left) (D.vnode right)
      undef <- D.eq result64 result32 >>= D.not
      result <- D.safeSll (D.vnode left) (D.vnode right)
      return $ D.VNode undef result D.Signed

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
cppShiftRight :: D.VNode -> D.VNode -> D.Verif D.VNode
cppShiftRight left right
  | D.isUnsigned (D.vtype left) = undefined
  | otherwise = undefined


