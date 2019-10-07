module DSL.Z3Wrapper where

import           Control.Monad.State.Strict (liftIO, unless)
import           Z3.Monad                   (MonadZ3)
import qualified Z3.Monad                   as Z

type Sort = Z.Sort
type Node = Z.AST
type AST = Z.AST


assert :: MonadZ3 z3 => AST -> z3 ()
assert a = do
  sort <- Z.getSort a
  kind <- Z.getSortKind sort
  a' <- case kind of
    Z.Z3_BOOL_SORT -> return a
    Z.Z3_BV_SORT -> do
      size <- Z.getBvSortSize sort
      unless (size == 1) $ error "Cannot assert multibit BV"
      bvTrue <- Z.mkBvNum 1 1
      Z.mkEq a bvTrue
    s              -> error $ unwords ["Can't assert sort", show s]
  Z.assert a'

typeSafeUnary' :: MonadZ3 z3 => AST -> z3 Z.Sort
typeSafeUnary' ast = do
  sort <- Z.getSort ast
  kind <- Z.getSortKind sort
  case kind of
    Z.Z3_BV_SORT -> return sort
    s            -> error $ unwords ["Expected BV sort, not", show s]

typeSafeBinary :: MonadZ3 z3 => String -> AST -> AST -> z3 ()
typeSafeBinary op ast1 ast2 = do
  s1 <- typeSafeUnary' ast1
  s2 <- typeSafeUnary' ast2
  size1 <- Z.getBvSortSize s1
  size2 <- Z.getBvSortSize s2
  unless (s1 == s2) $ error $ unwords [op, ": bit-widths must match"]

eq :: MonadZ3 z3 => AST -> AST -> z3 AST
eq = Z.mkEq

add :: MonadZ3 z3 => AST -> AST -> z3 AST
add a b = do
  typeSafeBinary "add" a b
  Z.mkBvadd a b

sub :: MonadZ3 z3 => AST -> AST -> z3 AST
sub a b = do
  typeSafeBinary "sub" a b
  Z.mkBvsub a b

mul :: MonadZ3 z3 => AST -> AST -> z3 AST
mul a b = do
  typeSafeBinary "mul" a b
  Z.mkBvmul a b

sdiv :: MonadZ3 z3 => AST -> AST -> z3 AST
sdiv a b = do
  typeSafeBinary "sdiv" a b
  Z.mkBvsdiv a b

udiv :: MonadZ3 z3 => AST -> AST -> z3 AST
udiv a b = do
  typeSafeBinary "udiv" a b
  Z.mkBvudiv a b

mod :: MonadZ3 z3 => AST -> AST -> z3 AST
mod a b = do
  typeSafeBinary "mod" a b
  Z.mkBvsmod a b

srem :: MonadZ3 z3 => AST -> AST -> z3 AST
srem a b = do
  typeSafeBinary "srem" a b
  Z.mkBvsrem a b

urem :: MonadZ3 z3 => AST -> AST -> z3 AST
urem a b = do
  typeSafeBinary "urem" a b
  Z.mkBvurem a b

and :: MonadZ3 z3 => AST -> AST -> z3 AST
and a b = do
  typeSafeBinary "and" a b
  Z.mkBvand a b

or :: MonadZ3 z3 => AST -> AST -> z3 AST
or a b = do
  typeSafeBinary "or" a b
  Z.mkBvor a b

xor :: MonadZ3 z3 => AST -> AST -> z3 AST
xor a b = do
  typeSafeBinary "xor" a b
  Z.mkBvxor a b

not :: MonadZ3 z3 => AST -> z3 AST
not = Z.mkBvnot

neg :: MonadZ3 z3 => AST -> z3 AST
neg = Z.mkBvneg

sll :: MonadZ3 z3 => AST -> AST -> z3 AST
sll a b = do
  typeSafeBinary "sll" a b
  Z.mkBvshl a b

srl :: MonadZ3 z3 => AST -> AST -> z3 AST
srl a b = do
  typeSafeBinary "srl" a b
  Z.mkBvlshr a b

sra :: MonadZ3 z3 => AST -> AST -> z3 AST
sra a b = do
  typeSafeBinary "sra" a b
  Z.mkBvashr a b

-- Comparisons

cmpWrapper :: MonadZ3 z3 => AST -> z3 AST
cmpWrapper a = do
  true <- Z.mkBvNum 1 1
  false <- Z.mkBvNum 1 0
  Z.mkIte a true false

ugt :: MonadZ3 z3 => AST -> AST -> z3 AST
ugt a b = do
  typeSafeBinary "ugt" a b
  Z.mkBvugt a b >>= cmpWrapper

sgt :: MonadZ3 z3 => AST -> AST -> z3 AST
sgt a b = do
  typeSafeBinary "sgt" a b
  Z.mkBvsgt a b >>= cmpWrapper

ugte :: MonadZ3 z3 => AST -> AST -> z3 AST
ugte a b = do
  typeSafeBinary "ugte" a b
  Z.mkBvuge a b >>= cmpWrapper

sgte :: MonadZ3 z3 => AST -> AST -> z3 AST
sgte a b = do
  typeSafeBinary "sgte" a b
  Z.mkBvsge a b >>= cmpWrapper

ult :: MonadZ3 z3 => AST -> AST -> z3 AST
ult a b = do
  typeSafeBinary "ult" a b
  Z.mkBvult a b >>= cmpWrapper

slt :: MonadZ3 z3 => AST -> AST -> z3 AST
slt a b = do
  typeSafeBinary "slt" a b
  Z.mkBvslt a b >>= cmpWrapper

ulte :: MonadZ3 z3 => AST -> AST -> z3 AST
ulte a b = do
  typeSafeBinary "ulte" a b
  Z.mkBvule a b >>= cmpWrapper

slte :: MonadZ3 z3 => AST -> AST -> z3 AST
slte a b = do
  typeSafeBinary "slte" a b
  Z.mkBvsle a b >>= cmpWrapper

iseq :: MonadZ3 z3 => AST -> AST -> z3 AST
iseq a b = do
  typeSafeBinary "iseq" a b
  Z.mkEq a b >>= cmpWrapper

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

push :: MonadZ3 z3 => z3 ()
push = Z.push

pop :: MonadZ3 z3 => z3 ()
pop = Z.pop 1

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

-- Floating point wrappers

double :: MonadZ3 z3 => Double -> z3 AST
double doub = do
  doubSort <- Z.mkDoubleSort
  Z.mkFpFromDouble doub doubSort

inf :: MonadZ3 z3 => Bool -> z3 AST
inf positive = do
  doubSort <- Z.mkDoubleSort
  Z.mkFpInf doubSort positive

fpzero ::MonadZ3 z3 => Bool -> z3 AST
fpzero positive = do
  doubSort <- Z.mkDoubleSort
  Z.mkFpZero doubSort positive

nan :: MonadZ3 z3 => z3 AST
nan = do
  doubSort <- Z.mkDoubleSort
  Z.mkFpNan doubSort

rna :: MonadZ3 z3 => z3 AST
rna = Z.mkFpRna

rne :: MonadZ3 z3 => z3 AST
rne = Z.mkFpRne

rtn :: MonadZ3 z3 => z3 AST
rtn = Z.mkFpRtn

rtp :: MonadZ3 z3 => z3 AST
rtp = Z.mkFpRtp

rtz :: MonadZ3 z3 => z3 AST
rtz = Z.mkFpRtz

isInf :: MonadZ3 z3 => AST -> z3 AST
isInf = Z.mkFpIsInf

isNan :: MonadZ3 z3 => AST -> z3 AST
isNan = Z.mkFpIsNan

isNeg :: MonadZ3 z3 => AST -> z3 AST
isNeg = Z.mkFpIsNeg

isPos :: MonadZ3 z3 => AST -> z3 AST
isPos = Z.mkFpIsPos

isZero :: MonadZ3 z3 => AST -> z3 AST
isZero = Z.mkFpIsZero

rmWrapper :: MonadZ3 z3
          => (AST -> AST -> AST -> z3 AST)
          -> AST
          -> AST
          -> z3 AST
rmWrapper op a b = do
  -- In the future we will get the current rounding mode from the monad
  rna_ <- rna
  op rna_ a b

fpAbs :: MonadZ3 z3 => AST -> z3 AST
fpAbs = Z.mkFpAbs

fpAdd :: MonadZ3 z3 => AST -> AST -> z3 AST
fpAdd = rmWrapper Z.mkFpAdd

fpSub :: MonadZ3 z3 => AST -> AST -> z3 AST
fpSub = rmWrapper Z.mkFpSub

fpDiv :: MonadZ3 z3 => AST -> AST -> z3 AST
fpDiv = rmWrapper Z.mkFpDiv

fpMul :: MonadZ3 z3 => AST -> AST -> z3 AST
fpMul = rmWrapper Z.mkFpMul

fpRem :: MonadZ3 z3 => AST -> AST -> z3 AST
fpRem = Z.mkFpRem

fpNeg :: MonadZ3 z3 => AST -> z3 AST
fpNeg = Z.mkFpNeg

fpEq :: MonadZ3 z3 => AST -> AST -> z3 AST
fpEq = Z.mkFpEq

fpGte :: MonadZ3 z3 => AST -> AST -> z3 AST
fpGte = Z.mkFpGeq

fpGt :: MonadZ3 z3 => AST -> AST -> z3 AST
fpGt = Z.mkFpGt

fpLte :: MonadZ3 z3 => AST -> AST -> z3 AST
fpLte = Z.mkFpLeq

fpLt :: MonadZ3 z3 => AST -> AST -> z3 AST
fpLt = Z.mkFpLt

fpMin :: MonadZ3 z3 => AST -> AST -> z3 AST
fpMin = Z.mkFpMin

fpMax :: MonadZ3 z3 => AST -> AST -> z3 AST
fpMax = Z.mkFpMax

