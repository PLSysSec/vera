module DSL where
import           BenchUtils
import           Control.Monad.State.Strict (liftIO)
import qualified DSL.DSL                    as D
import qualified DSL.Typed                  as T
import qualified DSL.Z3Wrapper              as Ops
import           IonMonkey.Objects
import           IonMonkey.Operations
import           Prelude                    hiding (and, not)
import           Test.Tasty.HUnit
import           Utils
import qualified Z3.Monad                   as M

dslTests :: BenchTest
dslTests = benchTestGroup "DSL" [ addTest
                                , incrTest
                                , incrTest2
                                , fpTest
                                ]

addTest :: BenchTest
addTest = benchTestCase "add" $ do

  r <- D.evalVerif Nothing $ do

         -- Should not segfault getting the model or in the addition
         one <- D.i32c 1
         two <- D.i32c 2
         added <- Ops.add one two
         result <- D.i32v "r"
         D.assign result added

         -- Should not segfault due to mismatched types
         result2 <- D.i1v "r2"
         result3 <- D.i1v "r3"
         Ops.slt one two >>= D.assign result2
         Ops.iseq one two >>= D.assign result3

         -- Cond died because it requires a bool type for its cond
         result4 <- D.i32v "r4"
         c <- Ops.cond result2 one two
         D.assign c result4

         -- Bizarrely these guys worked first try
         result5 <- D.i64v "r5"
         result6 <- D.i64v "r6"
         Ops.sext one 32 >>= D.assign result5
         Ops.uext one 32 >>= D.assign result6

         -- Shifts!
         result7 <- D.i32v "r7"
         result8 <- D.i32v "r8"
         result9 <- D.i32v "r9"
         Ops.safeSll one two >>= D.assign result7
         Ops.safeSrl one two >>= D.assign result8
         Ops.safeSra one two >>= D.assign result9

         -- Min and max
         result10 <- D.i32v "r10"
         result11 <- D.i32v "r11"
         result12 <- D.i32v "r12"
         result13 <- D.i32v "r13"
         D.smin one two >>= D.assign result10
         D.smax one two >>= D.assign result11
         D.umin one two >>= D.assign result12
         D.umax one two >>= D.assign result13
         D.runSolver

  satTest r


incrTest :: BenchTest
incrTest = benchTestCase "incr" $ do

  r <- D.evalVerif Nothing $ do
    one <- D.i32c 1
    two <- D.i32c 2
    added <- Ops.add one two
    result <- D.i32v "r"
    M.push
    D.assign result added
    r <- D.runSolver
    M.pop 1
    D.runSolver
    return r

  satTest r


incrTest2 :: BenchTest
incrTest2 = benchTestCase "incr2" $ do

  r <- D.evalVerif Nothing $ do
    M.push
    leftRange <- inputRange T.Signed "left start range"
    M.pop 1

    D.runSolver

  satTest r

fpTest :: BenchTest
fpTest = benchTestCase "fp" $ do

  r <- D.evalVerif Nothing $ do
    doubSort <- M.mkDoubleSort
    fp1 <- M.mkFpFromDouble 4.5 doubSort
    fp2 <- M.mkFpFromInt 4 doubSort
    z <- M.mkFpZero doubSort False
    nan <- M.mkFpNan doubSort
    inf <- M.mkFpInf doubSort True

    rna <- M.mkFpRna
    rne <- M.mkFpRne
    rtn <- M.mkFpRtn
    rtp <- M.mkFpRtp
    rtz <- M.mkFpRtz

    isInf <- M.mkFpIsInf z
    isNan <- M.mkFpIsNan nan
    isNeg <- M.mkFpIsNeg nan
    isPos <- M.mkFpIsPos fp1
    isZero <- M.mkFpIsZero z

    abs <- M.mkFpAbs fp1
    adds <- M.mkFpAdd rna fp1 fp2
    subs <- M.mkFpSub rne fp1 fp2
    divs <- M.mkFpDiv rtn fp1 fp2
    muls <- M.mkFpMul rtz nan nan
    rems <- M.mkFpRem fp1 fp2 -- NO IDEA
    negs <- M.mkFpNeg fp1
    eqs <- M.mkFpEq fp1 fp2
    geqs <- M.mkFpGeq fp1 fp2
    gts <- M.mkFpGt fp1 fp2
    leqs <- M.mkFpLeq fp1 fp2
    lts <- M.mkFpLt fp1 nan
    maxs <- M.mkFpMax fp1 fp2
    mins <- M.mkFpMin fp1 fp2

    D.runSolver

  satTest r
