module DSL where
import           BenchUtils
import           Control.Monad.State.Strict (liftIO)
import qualified Data.Map                   as Map
import           DSL.DSL                    (fpCeil, fpFloor)
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
dslTests = benchTestGroup "DSL" [ --addTest
                                -- , incrTest
                                -- , incrTest2
                                -- , fpTest
                                -- , fpRoundTest
                                fpTest
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

fpRoundTest :: BenchTest
fpRoundTest = benchTestCase "fp rounding" $ do
  r <- D.evalVerif Nothing $ do
    doubSort <- M.mkDoubleSort
    -- Make some stuff to round
    four5 <- M.mkFpFromDouble 4.5 doubSort
    minusFour5 <- M.mkFpFromDouble (-4.5) doubSort
    -- Make sure it rounds to the correct values
    r1 <- fpFloor four5
    r2 <- fpFloor minusFour5
    r3 <- fpCeil four5
    r4 <- fpCeil minusFour5
    D.doubv "r1" >>= D.assign r1
    D.doubv "r2" >>= D.assign r2
    D.doubv "r3" >>= D.assign r3
    D.doubv "r4" >>= D.assign r4

    T.runSolver
  -- works, our representation just doesnt parse floats yet
  vtest r $ Map.fromList [ ("r1", 4)
                         , ("r2", -5)
                         , ("r3", 5)
                         , ("r4", -4)
                         ]

fpTest :: BenchTest
fpTest = benchTestCase "fp" $ do

  r <- D.evalVerif Nothing $ do
    doubSort <- M.mkDoubleSort
    fp1 <- M.mkFpFromDouble (-4.2) doubSort
    D.doubv "fourFive" >>= D.assign fp1
    fp2 <- M.mkFpFromInt (-4) doubSort
    D.doubv "minusFour" >>= D.assign fp2
    z <- M.mkFpZero doubSort False
    nan <- M.mkFpNan doubSort
    inf <- M.mkFpInf doubSort True
    inf2 <- M.mkFpInf doubSort False
    D.doubv "nan" >>= D.assign nan
    D.doubv "neginf" >>= D.assign inf
    D.doubv "posinf" >>= D.assign inf2

    rna <- M.mkFpRna
    rne <- M.mkFpRne
    rtn <- M.mkFpRtn
    rtp <- M.mkFpRtp
    rtz <- M.mkFpRtz

    isInf <- D.isInf z
    D.i1v "zeroNotInf" >>= D.assign isInf
    isNan <- D.isNan nan
    D.i1v "isNan" >>= D.assign isNan
    isNeg <- D.isNeg nan
    D.i1v "nanIsNeg" >>= D.assign isNeg
    isPos <- D.isPos fp1
    D.i1v "fourFiveIsPos" >>= D.assign isPos
    isZero <- D.isZero z
    D.i1v "zeroIsZero" >>= D.assign isZero

    abs <- D.fpAbs fp2
    D.doubv "abs" >>= D.assign abs

    adds <- D.fpAdd fp1 fp2
    D.doubv "adds" >>= D.assign adds

    subs <- D.fpSub fp1 fp2
    D.doubv "subs" >>= D.assign subs

    muls <- D.fpMul nan nan
    D.doubv "muls" >>= D.assign muls

    negs <- D.fpNeg fp2
    D.doubv "negs" >>= D.assign negs

    eqs <- D.fpEq fp1 fp2
    D.i1v "eqs" >>= D.assign eqs

    geqs <- D.fpGte fp1 fp2
    D.i1v "geqs" >>= D.assign geqs

    gts <- D.fpGt fp1 fp2
    D.i1v "gts" >>= D.assign gts

    leqs <- D.fpLte fp1 fp2
    D.i1v "leq" >>= D.assign leqs

    lts <- D.fpLt fp1 nan
    D.i1v "lts" >>= D.assign lts

    maxs <- D.fpMax fp1 fp2
    D.doubv "maxs" >>= D.assign maxs

    mins <- D.fpMin fp1 fp2
    D.doubv "mins" >>= D.assign mins

    D.runSolver

  vtest r $ Map.fromList [ ("fourFive", -4.2)
                         , ("minusFour", -4.0)
                         , ("nan", 0/0)
                         , ("neginf", (-1/0))
                         , ("posinf", 1/0)
                         , ("zeroNotInf", 0)
                         , ("isNan", 1)
                         , ("nanIsNeg", 0)
                         , ("fourFiveIsPos", 0)
                         , ("zeroIsZero", 1)
                         , ("abs", 4)
                         , ("adds", -8.200000000000001)
                         , ("muls", 0/0)
                         , ("negs", 4)
                         , ("eqs", 0)
                         , ("geqs", 0)
                         , ("gts", 0)
                         , ("leq", 1)
                         , ("lts", 0)
                         , ("maxs", -4.0)
                         , ("mins", -4.2)
                         , ("subs", -0.20000000000000018)
                         ]


