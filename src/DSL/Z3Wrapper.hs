module DSL.Z3Wrapper where

import           Control.Monad.State.Strict (liftIO, unless)
import           Z3.Monad                   as Z

type Sort = Z.Sort
type Node = Z.AST


assert :: MonadZ3 z3 => AST -> z3 ()
assert = Z.assert

eq :: MonadZ3 z3 => AST -> AST -> z3 AST
eq = Z.mkEq

add :: MonadZ3 z3 => AST -> AST -> z3 AST
add = Z.mkBvadd

sub :: MonadZ3 z3 => AST -> AST -> z3 AST
sub = Z.mkBvsub

mul :: MonadZ3 z3 => AST -> AST -> z3 AST
mul = Z.mkBvmul

sdiv :: MonadZ3 z3 => AST -> AST -> z3 AST
sdiv = Z.mkBvsdiv

udiv :: MonadZ3 z3 => AST -> AST -> z3 AST
udiv = Z.mkBvudiv

mod :: MonadZ3 z3 => AST -> AST -> z3 AST
mod = Z.mkBvsmod

srem :: MonadZ3 z3 => AST -> AST -> z3 AST
srem = Z.mkBvsrem

urem :: MonadZ3 z3 => AST -> AST -> z3 AST
urem = Z.mkBvurem

and :: MonadZ3 z3 => AST -> AST -> z3 AST
and = Z.mkBvand

or :: MonadZ3 z3 => AST -> AST -> z3 AST
or = Z.mkBvor

xor :: MonadZ3 z3 => AST -> AST -> z3 AST
xor = Z.mkBvxor

not :: MonadZ3 z3 => AST -> z3 AST
not = Z.mkBvnot

neg :: MonadZ3 z3 => AST -> z3 AST
neg = Z.mkBvneg

sll :: MonadZ3 z3 => AST -> AST -> z3 AST
sll = Z.mkBvshl

srl :: MonadZ3 z3 => AST -> AST -> z3 AST
srl = Z.mkBvlshr

sra :: MonadZ3 z3 => AST -> AST -> z3 AST
sra = Z.mkBvashr

-- Comparisons

cmpWrapper :: MonadZ3 z3 => AST -> z3 AST
cmpWrapper a = Z.mkInt2bv 1 a

ugt :: MonadZ3 z3 => AST -> AST -> z3 AST
ugt a b = Z.mkBvugt a b >>= cmpWrapper

sgt :: MonadZ3 z3 => AST -> AST -> z3 AST
sgt a b = Z.mkBvsgt a b >>= cmpWrapper

ugte :: MonadZ3 z3 => AST -> AST -> z3 AST
ugte a b = Z.mkBvuge a b >>= cmpWrapper

sgte :: MonadZ3 z3 => AST -> AST -> z3 AST
sgte a b = Z.mkBvsge a b >>= cmpWrapper

ult :: MonadZ3 z3 => AST -> AST -> z3 AST
ult a b = Z.mkBvult a b >>= cmpWrapper

slt :: MonadZ3 z3 => AST -> AST -> z3 AST
slt a b = Z.mkBvslt a b >>= cmpWrapper

ulte :: MonadZ3 z3 => AST -> AST -> z3 AST
ulte a b = Z.mkBvule a b >>= cmpWrapper

slte :: MonadZ3 z3 => AST -> AST -> z3 AST
slte a b = Z.mkBvsle a b >>= cmpWrapper

iseq :: MonadZ3 z3 => AST -> AST -> z3 AST
iseq a b = Z.mkEq a b >>= cmpWrapper

cond :: MonadZ3 z3 => AST -> AST -> AST -> z3 AST
cond c a b = do
  bvTrue <- Z.mkBvNum 1 1
  isTrue <- Z.mkEq c bvTrue
  Z.mkIte isTrue a b

sext :: MonadZ3 z3 => AST -> Int -> z3 AST
sext a i = Z.mkZeroExt i a

uext :: MonadZ3 z3 => AST -> Int -> z3 AST
uext a i = Z.mkZeroExt i a

slice :: MonadZ3 z3 => AST -> Int -> Int -> z3 AST
slice a i1 i2 = Z.mkExtract i1 i2 a

-- rol :: MonadZ3 z3 => AST -> AST -> z3 AST
-- rol = mkExtRotateLeft

-- ror :: MonadZ3 z3 => AST -> AST -> z3 AST
-- ror = mkExtRotateRight

push :: MonadZ3 z3 => Int -> z3 ()
push _ = solverPush

pop :: MonadZ3 z3 => Int -> z3 ()
pop = solverPop

-- | Safe boolector shift operations
--
-- Boolector puts restrictions on the widths of the arguments to shift operations.
-- As of Boolector 3, the widths of both operands must be the same.
-- As of Boolectors < 3, the width of the first operand had to be a power of 2,
-- and the width of the second operand had to be log 2 of the first.
-- We DO NOT support the Boolector < 3 restriction
safeSll, safeSrl, safeSra {-, safeRol, safeRor -} :: MonadZ3 m => AST -> AST -> m AST
safeSll = shiftWrapper sll
safeSrl = shiftWrapper srl
safeSra = shiftWrapper sra
-- safeRol = shiftWrapper rol
-- safeRor = shiftWrapper ror

-- | Wrapper for boolector shift operations
shiftWrapper :: (MonadZ3 m)
             => (AST -> AST -> m AST) -- ^ Shift op
             -> AST -- ^ Thing to shift
             -> AST -- ^ Thing to shift by
             -> m AST -- ^ Result
shiftWrapper shiftOp toShift shiftVal = do
  toShiftSort <- Z.getSort toShift
  castVal <- Z.getBvSortSize toShiftSort >>= castToWidth shiftVal
  shiftOp toShift castVal

-- | Cast a node to a new width
-- If the new width is larger, use unsigned extension
-- If the new width is smaller, slice
castToWidth :: (MonadZ3 m)
            => Node -- ^ Node to be cast
            -> Int -- ^ New width
            -> m Node -- ^ Result
castToWidth varToCast newWidth = do
  sort <- Z.getSort varToCast
  sortKind <- Z.getSortKind sort
  let isBv = {- sortKind == Z.Z3_BOOL_SORT || -} sortKind == Z.Z3_BV_SORT
  unless isBv $ error $ "Should never cast non-bitvector sort (" ++ show sort ++ ")"
  oldWidth' <- Z.getBvSortSize sort
  let oldWidth = fromIntegral oldWidth'
  case compare oldWidth newWidth of
    LT -> uext varToCast (newWidth - oldWidth)
    GT -> slice varToCast (newWidth - 1) 0
    _  -> return varToCast

