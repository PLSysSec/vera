module DSL.Z3Wrapper where

import           Z3.Monad as Z

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

uge :: MonadZ3 z3 => AST -> AST -> z3 AST
uge = Z.mkBvuge

sge :: MonadZ3 z3 => AST -> AST -> z3 AST
sge = Z.mkBvsge

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
