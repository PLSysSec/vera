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


