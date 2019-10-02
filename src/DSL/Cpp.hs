module DSL.Cpp ( intMax
               , intMin
               , uintMax
               , uintMin
               , cppEq
               , cppOr
               , cppAnd
               , cppMin
               , cppMax
               , cppGt
               , cppGte
               , cppLt
               , cppLte
               , cppShiftLeft
               , cppShiftRight
               , vassign
               , vassert
               , module DSL.Typed
               ) where
import           DSL.DSL   hiding ()
import qualified DSL.DSL   as D
import           DSL.Typed

--
-- Variables and constants
--

intMax :: D.Verif VNode
intMax = newSignedNumber 2147483647

intMin :: D.Verif VNode
intMin = newSignedNumber (-2147483648)

uintMax :: D.Verif VNode
uintMax = newUnsignedNumber 4294967295

uintMin :: D.Verif VNode
uintMin = newUnsignedNumber 0

--
-- Operations
--

noopWrapper :: VNode
            -> VNode
            -> (D.Node -> D.Node -> D.Verif D.Node)
            -> D.Verif VNode
noopWrapper left right op = do
  result <- op (vnode left) (vnode right)
  let ty = if isUnsigned left && isUnsigned right then Unsigned else Signed
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
  | isUnsigned right && isUnsigned left = noopWrapper left right D.umin
  | isSigned right && isSigned left = noopWrapper left right D.smin
  | otherwise = error "Compiler error: Can't use std:min on a signed and unsigned"

cppMax :: VNode
       -> VNode
       -> D.Verif VNode
cppMax right left
  | isUnsigned right && isUnsigned left = noopWrapper left right D.umax
  | isSigned right && isSigned left = noopWrapper left right D.smax
  | otherwise = error "Compiler error: Can't use std:max on a signed and unsigned"

cppCompareWrapper :: VNode
                  -> VNode
                  -> (D.Node -> D.Node -> D.Verif D.Node)
                  -> (D.Node -> D.Node -> D.Verif D.Node)
                  -> D.Verif VNode
cppCompareWrapper left right uCompare sCompare
 | isUnsigned left || isUnsigned right = do
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
-- TODO: ARE WE DOING THIS RIGHT?
--
-- For negative a, the behavior of a << b is undefined.
--
-- Via Clang interpreter typeid function:
--
-- int = int << anything
-- uint = uint << anything
cppShiftLeft :: VNode -> VNode -> D.Verif VNode
cppShiftLeft left right
  | isUnsigned left = do
      result <- D.safeSll (vnode left) (vnode right)
      newMaybeDefinedNode left right result Unsigned
  | otherwise = do

      -- Do the operation in 64-bits and see if any of the left bits are set.
      -- If so, the operation has undefined behavior because some bit was
      -- shifted off the end of the 32-bit variable
      left64 <- D.uext (vnode left) 32
      right64 <- D.uext (vnode right) 32
      result64 <- D.safeSll left64 right64
      top32 <- D.slice result64 32 31
      zero <- D.i32c 0

      -- Is it undef?
      opUndef <- D.eq top32 zero >>= D.not
      parentsUndef <- D.or (vundef left) (vundef right)
      undef <- D.or opUndef parentsUndef

      result <- D.safeSll (vnode left) (vnode right)
      return $ newTempNode undef result Signed

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
cppShiftRight :: VNode -> VNode -> D.Verif VNode
cppShiftRight left right
  | isUnsigned left = do
      result <- D.safeSra (vnode left) (vnode right)
      newMaybeDefinedNode left right result Unsigned
  | otherwise = do

      -- if left is negative, the behavior is impl-defined
      zero <- D.i32c 0
      opUndef <- D.slt (vnode left) zero
      parentsUndef <- D.or (vundef left) (vundef right)
      undef <- D.or opUndef parentsUndef

      -- actually do the op
      result <- D.safeSra (vnode left) (vnode right)
      return $ newTempNode undef result Signed


