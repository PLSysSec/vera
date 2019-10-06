module DSL.Z3Wrapper where

import           Z3.Monad as Z
import           Control.Monad.State.Strict (unless)

type Sort = Z.Sort
type Node = Z.AST


assert :: MonadZ3 z3 => AST -> z3 ()
assert = Z.assert

eq :: MonadZ3 z3 => AST -> AST -> z3 AST
eq = Z.mkEq

add :: MonadZ3 z3 => AST -> AST -> z3 AST
add x y = Z.mkAdd [x,y]

sub :: MonadZ3 z3 => AST -> AST -> z3 AST
sub x y = Z.mkSub [x,y]

mul :: MonadZ3 z3 => AST -> AST -> z3 AST
mul x y = Z.mkMul [x,y]

div :: MonadZ3 z3 => AST -> AST -> z3 AST
div = Z.mkDiv

mod :: MonadZ3 z3 => AST -> AST -> z3 AST
mod = Z.mkMod

rem :: MonadZ3 z3 => AST -> AST -> z3 AST
rem = Z.mkRem

and :: MonadZ3 z3 => AST -> AST -> z3 AST
and x y = Z.mkAnd [x,y]

or :: MonadZ3 z3 => AST -> AST -> z3 AST
or x y = Z.mkOr [x,y]

xor :: MonadZ3 z3 => AST -> AST -> z3 AST
xor = Z.mkXor

not :: MonadZ3 z3 => AST -> z3 AST
not = Z.mkNot

neg :: MonadZ3 z3 => AST -> z3 AST
neg = Z.mkBvneg

sll :: MonadZ3 z3 => AST -> AST -> z3 AST
sll = Z.mkBvshl

srl :: MonadZ3 z3 => AST -> AST -> z3 AST
srl = Z.mkBvlshr

sra :: MonadZ3 z3 => AST -> AST -> z3 AST
sra = Z.mkBvashr

-- Comparisons

ugt :: MonadZ3 z3 => AST -> AST -> z3 AST
ugt = Z.mkBvugt

sgt :: MonadZ3 z3 => AST -> AST -> z3 AST
sgt = Z.mkBvsgt

ugte :: MonadZ3 z3 => AST -> AST -> z3 AST
ugte = Z.mkBvuge

sgte :: MonadZ3 z3 => AST -> AST -> z3 AST
sgte = Z.mkBvsge

ult :: MonadZ3 z3 => AST -> AST -> z3 AST
ult = Z.mkBvult

slt :: MonadZ3 z3 => AST -> AST -> z3 AST
slt = Z.mkBvslt

ulte :: MonadZ3 z3 => AST -> AST -> z3 AST
ulte = Z.mkBvule

slte :: MonadZ3 z3 => AST -> AST -> z3 AST
slte = Z.mkBvsle

cond :: MonadZ3 z3 => AST -> AST -> AST -> z3 AST
cond = Z.mkIte

sext :: MonadZ3 z3 => AST -> Int -> z3 AST
sext a i = Z.mkZeroExt i a

uext :: MonadZ3 z3 => AST -> Int -> z3 AST
uext a i = Z.mkZeroExt i a

slice :: MonadZ3 z3 => AST -> Int -> Int -> z3 AST
slice a i1 i2 = Z.mkExtract i1 i2 a

rol :: MonadZ3 z3 => AST -> AST -> z3 AST
rol = mkExtRotateLeft

ror :: MonadZ3 z3 => AST -> AST -> z3 AST
ror = mkExtRotateRight

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
safeSll, safeSrl, safeSra, safeRol, safeRor :: MonadZ3 m => AST -> AST -> m AST
safeSll = shiftWrapper sll
safeSrl = shiftWrapper srl
safeSra = shiftWrapper sra
safeRol = shiftWrapper rol
safeRor = shiftWrapper ror

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

