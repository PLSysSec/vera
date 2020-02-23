module Cpp (cppTests) where
import           ActiveCode.Cpp
import           BenchUtils
import           Data.Int
import qualified Data.Map                as M
import           Data.Word
import           DSL.DSL                 as D
import qualified DSL.Typed               as T
import           Prelude                 hiding (and, max, min, not, pi)
import qualified Test.QuickCheck.Monadic as Q
import qualified Test.Tasty.QuickCheck   as Q
import           Utils

cppTests :: BenchTest
cppTests = benchTestGroup "C++ tests" [
  benchTestGroup "Unit tests" [--  cppMinTest
                              -- , cppMaxTest
                              -- , cppCmpTest
                              -- , cppShlTest
                              -- , cppShrTest
                              -- , fpTest
                              -- , cppOverflowTest
                               fpExpTest
                              ]
  -- , benchTestGroup "QuickCheck tests on doubles" [
  --     benchTestGroup "Arithmetic binary ops" [ cppBinDoubleOpTest CppAdd
  --                                            , cppBinDoubleOpTest CppSub
  --                                            , cppBinDoubleOpTest CppMul
  --                                            , cppBinDoubleOpTest CppMin
  --                                            , cppBinDoubleOpTest CppMax
  --                                            ],
  --     benchTestGroup "Comparison ops" [ cppCmpDoubleOpTest CppGt
  --                                     , cppCmpDoubleOpTest CppGte
  --                                     , cppCmpDoubleOpTest CppLt
  --                                     , cppCmpDoubleOpTest CppLte
  --                                     ]
  --   , benchTestGroup "Unary ops" [ cppUniDoubleOpTest CppAbs
  --                                , cppUniDoubleOpTest CppNeg ]
  --   ],
  -- benchTestGroup "QuickCheck tests on int32s" [
  --     benchTestGroup "Arithmetic binary ops" [ cppBinI32OpTest CppAdd
  --                                            , cppBinI32OpTest CppSub
  --                                            , cppBinI32OpTest CppMul
  --                                            , cppBinI32OpTest CppMin
  --                                            , cppBinI32OpTest CppMax
  --                                            ],
  --     benchTestGroup "Comparison ops" [ cppCmpI32OpTest CppGt
  --                                     , cppCmpI32OpTest CppGte
  --                                     , cppCmpI32OpTest CppLt
  --                                     , cppCmpI32OpTest CppLte
  --                                     ],
  --     benchTestGroup "Unary ops" [ cppUniI32OpTest CppAbs
  --                                , cppUniI32OpTest CppNeg ]
  --   ],
  -- benchTestGroup "QuickCheck tests on int16s" [
  --     benchTestGroup "Arithmetic binary ops" [ cppBinI16OpTest CppAdd
  --                                            , cppBinI16OpTest CppSub
  --                                            , cppBinI16OpTest CppMul
  --                                            , cppBinI16OpTest CppMin
  --                                            , cppBinI16OpTest CppMax
  --                                            ],
  --     benchTestGroup "Comparison ops" [ cppCmpI16OpTest CppGt
  --                                     , cppCmpI16OpTest CppGte
  --                                     , cppCmpI16OpTest CppLt
  --                                     , cppCmpI16OpTest CppLte
  --                                     ],
  --     benchTestGroup "Unary ops" [ -- cppUniI16OpTest CppAbs
  --                                  cppUniI16OpTest CppNeg ]
  --   ],
  -- benchTestGroup "QuickCheck tests on int8s" [
  --     benchTestGroup "Arithmetic binary ops" [ cppBinI8OpTest CppAdd
  --                                            , cppBinI8OpTest CppSub
  --                                            , cppBinI8OpTest CppMul
  --                                            , cppBinI8OpTest CppMin
  --                                            , cppBinI8OpTest CppMax
  --                                            ],
  --     benchTestGroup "Comparison ops" [ cppCmpI8OpTest CppGt
  --                                     , cppCmpI8OpTest CppGte
  --                                     , cppCmpI8OpTest CppLt
  --                                     , cppCmpI8OpTest CppLte
  --                                     ],
  --     benchTestGroup "Unary ops" [ -- cppUniI8OpTest CppAbs
  --                                 cppUniI8OpTest CppNeg ]
  --   ],
  -- benchTestGroup "QuickCheck tests on uint32s" [
  --     benchTestGroup "Arithmetic binary ops" [ cppBinW32OpTest CppAdd
  --                                            , cppBinW32OpTest CppSub
  --                                            , cppBinW32OpTest CppMul
  --                                            , cppBinW32OpTest CppMin
  --                                            , cppBinW32OpTest CppMax
  --                                            ],
  --     benchTestGroup "Comparison ops" [ cppCmpW32OpTest CppGt
  --                                     , cppCmpW32OpTest CppGte
  --                                     , cppCmpW32OpTest CppLt
  --                                     , cppCmpW32OpTest CppLte
  --                                     ],
  --     benchTestGroup "Unary ops" [ --cppUniW32OpTest CppAbs
  --                                  cppUniW32OpTest CppNeg ]
  --   ],
  -- benchTestGroup "QuickCheck tests on uint16s" [
  --     benchTestGroup "Arithmetic binary ops" [ cppBinW16OpTest CppAdd
  --                                            , cppBinW16OpTest CppSub
  --                                            , cppBinW16OpTest CppMul
  --                                            , cppBinW16OpTest CppMin
  --                                            , cppBinW16OpTest CppMax
  --                                            ],
  --     benchTestGroup "Comparison ops" [ cppCmpW16OpTest CppGt
  --                                     , cppCmpW16OpTest CppGte
  --                                     , cppCmpW16OpTest CppLt
  --                                     , cppCmpW16OpTest CppLte
  --                                     ],
  --     benchTestGroup "Unary ops" [ --cppUniW16OpTest CppAbs
  --                                  cppUniW16OpTest CppNeg ]
  --   ],
  -- benchTestGroup "QuickCheck tests on uint8s" [
  --     benchTestGroup "Arithmetic binary ops" [ cppBinW8OpTest CppAdd
  --                                            , cppBinW8OpTest CppSub
  --                                            , cppBinW8OpTest CppMul
  --                                            , cppBinW8OpTest CppMin
  --                                            , cppBinW8OpTest CppMax
  --                                            ],
  --     benchTestGroup "Comparison ops" [ cppCmpW8OpTest CppGt
  --                                     , cppCmpW8OpTest CppGte
  --                                     , cppCmpW8OpTest CppLt
  --                                     , cppCmpW8OpTest CppLte
  --                                     ],
  --     benchTestGroup "Unary ops" [ -- cppUniW8OpTest CppAbs
  --                                 cppUniW8OpTest CppNeg ]
  --   ]
  ]

trueBit :: Double
trueBit = 1

falseBit :: Double
falseBit = 0

negOne :: Double
negOne = 4294967295

cppOverflowTest :: BenchTest
cppOverflowTest = benchTestCase "overflow test" $ do
  r <- T.evalVerif Nothing $ do
    _1 <- T.num 0
    big <- T.num 270205441
    result <- T.cppAdd _1 big
    let undef = T.vundef result
    D.i1v "undef_bit" >>= D.assign undef
    T.runSolver

  vtest r $ M.fromList [ ("undef_bit", 0)
                       ]
fpExpTest :: BenchTest
fpExpTest = benchTestCase "exp" $ do

  r <- T.evalVerif Nothing $ do

    one <- T.fpnum (-2)
    exp <- T.getFpExponent one
    res <- T.uint16 "exp"
    T.vassign exp res

    T.runSolver

  vtest r $ M.fromList [ ("exp", 1023) ]


cppMinTest :: BenchTest
cppMinTest = benchTestCase "min test" $ do

  r <- T.evalVerif Nothing $ do

    -- Check that cppMin is aware of the sign for signed numbers
    one <- T.num 1
    minusOne <- T.num (-1)
    result <- T.int32 "result"
    min' <- T.cppMin one minusOne
    T.vassign result min'

    -- Check that cppMin does the right thing with unsigned numbers
    uMinusOne <- T.unum (-1)
    uOne <- T.unum 1
    umin' <- T.cppMin uMinusOne uOne
    uresult <- T.uint32 "uresult"
    T.vassign uresult umin'

    T.runSolver

  vtest r $ M.fromList [ ("result", negOne)
                       , ("uresult", 1)
                       ]

cppMaxTest :: BenchTest
cppMaxTest = benchTestCase "max test" $ do

  r <- T.evalVerif Nothing $ do

    -- Check that cppMax is aware of the sign for signed numbers
    one <- T.num 1
    minusOne <- T.num (-1)
    result <- T.int32 "result"
    min <- T.cppMax one minusOne
    T.vassign result min

    -- Check that cppMin does the right thing with unsigned numbers
    uMinusOne <- T.unum (-1)
    uOne <- T.unum 1
    umin' <- T.cppMax uMinusOne uOne
    uresult <- T.uint32 "uresult"
    T.vassign uresult umin'

    T.runSolver

  vtest r $ M.fromList [ ("result", 1)
                       , ("uresult", negOne)
                       ]

cppCmpTest :: BenchTest
cppCmpTest = benchTestCase "cmp test" $ do

  r <- T.evalVerif Nothing $ do

    -- Make sure it doesn't segfault on equality comparison
    five <- T.num 5
    T.cppEq five five >>= T.vassert

    -- Make sure it uses a signed comparison for two signed numbers
    one <- T.num 1
    minusOne <- T.num (-1)
    result1 <- T.bool "result1"
    result2 <- T.bool "result2"
    result3 <- T.bool "result3"
    result4 <- T.bool "result4"
    gt <- T.cppGt one minusOne
    gte <- T.cppGte one minusOne
    lt <- T.cppLt one minusOne
    lte <- T.cppLte one minusOne
    T.vassign result1 gt
    T.vassign result2 gte
    T.vassign result3 lt
    T.vassign result4 lte

    -- Make sure that it uses an unsigned comparison for an unsigned and signed,
    -- unsigned and unsigned
    uMinusOne <- T.unum (-1)
    uOne <- T.unum 1

    result5 <- T.bool "result5"
    result6 <- T.bool "result6"
    result7 <- T.bool "result7"
    result8 <- T.bool "result8"
    result9 <- T.bool "result9"
    result10 <- T.bool "result10"
    result11 <- T.bool "result11"
    result12 <- T.bool "result12"

    bgt <- T.cppGt one uMinusOne
    bgte <- T.cppGte one uMinusOne
    blt <- T.cppLt one uMinusOne
    blte <- T.cppLte one uMinusOne

    T.vassign result5 bgt
    T.vassign result6 bgte
    T.vassign result7 blt
    T.vassign result8 blte

    ugt' <- T.cppGt uOne uMinusOne
    ugte' <- T.cppGte uOne uMinusOne
    ult' <- T.cppLt uOne uMinusOne
    ulte' <- T.cppLte uOne uMinusOne

    T.vassign result9 ugt'
    T.vassign result10 ugte'
    T.vassign result11 ult'
    T.vassign result12 ulte'

    T.runSolver

  vtest r $ M.fromList [ ("result1", trueBit)
                       , ("result2", trueBit)
                       , ("result3", falseBit)
                       , ("result4", falseBit)
                       , ("result5", falseBit)
                       , ("result6", falseBit)
                       , ("result7", trueBit)
                       , ("result8", trueBit)
                       , ("result9", falseBit)
                       , ("result10", falseBit)
                       , ("result11", trueBit)
                       , ("result12", trueBit)
                       ]

cppShlTest :: BenchTest
cppShlTest = benchTestCase "shl test" $ do

  r <- T.evalVerif Nothing $ do

    -- Shift of a negative should be undefined
    minusOne <- T.num (-1)
    two <- T.num 2
    shift1 <- T.cppShiftLeft minusOne two

    result1 <- T.int32 "result1"
    T.vassign result1 shift1

    -- Shift of any bit off the left end of the var should be undefined
    one <- T.num 1
    thirtyThree <- T.num 33
    shift2 <- T.cppShiftLeft one thirtyThree

    result2 <- T.int32 "result2"
    T.vassign result2 shift2

    -- Shift of any bit wayyyyy of the left end of the var should be undefined
    fourHundred <- T.num 400
    shift3 <- T.cppShiftLeft one fourHundred

    result3 <- T.int32 "result3"
    T.vassign result3 shift3

    -- Shift by a negative should be undefined
    shift4 <- T.cppShiftLeft one minusOne
    result4 <- T.int32 "result4"
    T.vassign result4 shift4

    -- Shift of bits out of an unsigned should not be undef
    uone <- T.unum 1
    shift5 <- T.cppShiftLeft uone fourHundred
    result5 <- T.uint32 "result5"
    T.vassign result5 shift5

    -- A normal shift of the result of an undef operation should still be undef
    shift6 <- T.cppShiftLeft shift3 one
    result6 <- T.int32 "result6"
    T.vassign result6 shift6

    -- Shift of an unsigned by a negative should be undef
    shift7 <- T.cppShiftLeft uone minusOne
    result7 <- T.uint32 "result7"
    T.vassign result7 shift7

    -- A normal shift of the result of an unsigned undef op should still be undef
    shift8 <- T.cppShiftLeft shift7 uone
    result8 <- T.uint32 "result8"
    T.vassign result8 shift8

    T.runSolver

  vtest r $ M.fromList [ ("result1_undef", trueBit)
                       , ("result2_undef", trueBit)
                       , ("result3_undef", trueBit)
                       , ("result4_undef", trueBit)
                       , ("result5_undef", falseBit)
                       , ("result6_undef", trueBit)
                       , ("result7_undef", trueBit)
                       , ("result8_undef", trueBit)
                       ]

cppShrTest :: BenchTest
cppShrTest = benchTestCase "shr test" $ do

  r <- T.evalVerif Nothing $ do

    -- Shifting an unsigned by a negative should be undef
    minusOne <- T.num (-1)
    uOne <- T.unum 1
    shift1 <- T.cppShiftRight uOne minusOne
    result1 <- T.uint32 "result1"
    T.vassign result1 shift1

    -- Shifting a signed by a negative should be undef
    one <- T.num 1
    shift2 <- T.cppShiftRight one minusOne
    result2 <- T.int32 "result2"
    T.vassign result2 shift2

    -- Shifting an unsigned should result in a logical shift
    uMinusOne <- T.unum (-1)
    thirtyOne <- T.unum 31
    shift3 <- T.cppShiftRight uMinusOne thirtyOne
    result3 <- T.uint32 "result3"
    T.vassign shift3 result3

    -- Shifting a signed should result in an arithmetic shift
    shift4 <- T.cppShiftRight minusOne thirtyOne
    result4 <- T.int32 "result4"
    T.vassign shift4 result4

    -- Shifting an undef thing should result in an undef thing
    shift5 <- T.cppShiftRight shift2 one
    result5 <- T.int32 "result5"
    T.vassign shift5 result5

    T.runSolver

  vtest r $ M.fromList [ ("result1_undef", trueBit)
                       , ("result2_undef", trueBit)
                       , ("result3", 1)
                       , ("result4", negOne)
                       , ("result5_undef", trueBit)
                       ]

fpTest :: BenchTest
fpTest = benchTestCase "fp test" $ do

  r <- T.evalVerif Nothing $ do

    v <- T.fp "fp var"
    c <- T.fpnum 5.6

    _added <- T.jsAdd v c
    _subbed <- T.jsSub v c
    _mulled <- T.jsMul v c
    pi <- T.posInf
    ni <- T.negInf
    pz <- T.posZero
    nz <- T.negZero
    n <- T.nan

    _t1 <- T.isInf pi
    _t2 <- T.isInf ni
    _t3 <- T.isNan n
    _t4 <- T.isNeg ni
    _t5 <- T.isNeg nz
    _t6 <- T.isPos pi
    _t7 <- T.isPos pz
    _t8 <- T.isZero pz
    _t9 <- T.isZero nz

    T.runSolver

  vtest r $ M.fromList []

cppBinDoubleOpTest bop = benchTestProperty ("QuickCheck " ++ show bop) cppT
  where cppT :: Double -> Double -> Q.Property
        cppT x y = Q.monadicIO $ do
          cppRes <- Q.run $ cppBin bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.fpnum x
              yv <- T.fpnum y
              T.named "result" $ (cppBinOpToFunc bop) xv yv
              T.runSolver
          let (Just smtRes) = M.lookup "result" vars
          Q.assert $ smtRes == cppRes

cppCmpDoubleOpTest bop = benchTestProperty ("QuickCheck " ++ show bop) cppT
  where cppT :: Double -> Double -> Q.Property
        cppT x y = Q.monadicIO $ do
          cppRes <- Q.run $ cppBin bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.fpnum x
              yv <- T.fpnum y
              T.named "result" $ (cppBinOpToFunc bop) xv yv
              T.runSolver
          let (Just smtRes) = M.lookup "result" vars
          Q.assert $ (smtRes == 1) == cppRes


cppUniDoubleOpTest uop = benchTestProperty ("QuickCheck " ++ show uop) cppT
  where cppT :: Double -> Q.Property
        cppT x = Q.monadicIO $ do
          cppRes <- Q.run $ cppUni uop x
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.fpnum x
              T.named "result" $ (cppUniOpToFunc uop) xv
              T.runSolver
          let (Just smtRes) = M.lookup "result" vars
          Q.assert $ smtRes == cppRes

cppBinI32OpTest bop = benchTestProperty ("QuickCheck " ++ show bop) cppT
  where cppT :: Int32 -> Int32 -> Q.Property
        cppT x y = Q.monadicIO $ do
          cppRes <- Q.run $ cppBin bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.named "x" $ T.num $ toInteger x
              yv <- T.named "y" $ T.num $ toInteger y
              smtRes <- T.named "smtRes" $ (cppBinOpToFunc bop) xv yv
              cppResV <- T.named "cppRes" $ T.num $ toInteger (cppRes :: Int32)
              ok <- D.iseq (T.vnode smtRes) (T.vnode cppResV)
              D.named "ok" ok
              T.runSolver
          let (Just ok) = M.lookup "ok" vars
          Q.assert $ ok == 1

cppCmpI32OpTest bop = benchTestProperty ("QuickCheck " ++ show bop) cppT
  where cppT :: Int32 -> Int32 -> Q.Property
        cppT x y = Q.monadicIO $ do
          cppRes <- Q.run $ cppBin bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.num $ toInteger x
              yv <- T.num $ toInteger y
              T.named "result" $ (cppBinOpToFunc bop) xv yv
              T.runSolver
          let (Just smtRes) = M.lookup "result" vars
          Q.assert $ (smtRes == 1) == cppRes


cppUniI32OpTest bop = benchTestProperty ("QuickCheck " ++ show bop) cppT
  where cppT :: Int32 -> Int32 -> Q.Property
        cppT x y = Q.monadicIO $ do
          cppRes <- Q.run $ cppBin bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.named "x" $ T.num $ toInteger x
              smtRes <- T.named "smtRes" $ (cppUniOpToFunc bop) xv
              cppResV <- T.named "cppRes" $ T.num $ toInteger (cppRes :: Int32)
              ok <- D.iseq (T.vnode smtRes) (T.vnode cppResV)
              D.named "ok" ok
              T.runSolver
          let (Just ok) = M.lookup "ok" vars
          Q.assert $ ok == 1

cppBinI16OpTest bop = benchTestProperty ("QuickCheck " ++ show bop) cppT
  where cppT :: Int16 -> Int16 -> Q.Property
        cppT x y = Q.monadicIO $ do
          cppRes <- Q.run $ cppBin bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.named "x" $ T.num16 $ toInteger x
              yv <- T.named "y" $ T.num16 $ toInteger y
              smtRes <- T.named "smtRes" $ (cppBinOpToFunc bop) xv yv
              cppResV <- T.named "cppRes" $ T.num16 $ toInteger (cppRes :: Int16)
              ok <- D.iseq (T.vnode smtRes) (T.vnode cppResV)
              D.named "ok" ok
              T.runSolver
          let (Just ok) = M.lookup "ok" vars
          Q.assert $ ok == 1

cppCmpI16OpTest bop = benchTestProperty ("QuickCheck " ++ show bop) cppT
  where cppT :: Int16 -> Int16 -> Q.Property
        cppT x y = Q.monadicIO $ do
          cppRes <- Q.run $ cppBin bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.num16 $ toInteger x
              yv <- T.num16 $ toInteger y
              T.named "result" $ (cppBinOpToFunc bop) xv yv
              T.runSolver
          let (Just smtRes) = M.lookup "result" vars
          Q.assert $ (smtRes == 1) == cppRes


cppUniI16OpTest bop = benchTestProperty ("QuickCheck " ++ show bop) cppT
  where cppT :: Int16 -> Int16 -> Q.Property
        cppT x y = Q.monadicIO $ do
          cppRes <- Q.run $ cppBin bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.named "x" $ T.num16 $ toInteger x
              smtRes <- T.named "smtRes" $ (cppUniOpToFunc bop) xv
              cppResV <- T.named "cppRes" $ T.num16 $ toInteger (cppRes :: Int16)
              ok <- D.iseq (T.vnode smtRes) (T.vnode cppResV)
              D.named "ok" ok
              T.runSolver
          let (Just ok) = M.lookup "ok" vars
          Q.assert $ ok == 1

cppBinI8OpTest bop = benchTestProperty ("QuickCheck " ++ show bop) cppT
  where cppT :: Int8 -> Int8 -> Q.Property
        cppT x y = Q.monadicIO $ do
          cppRes <- Q.run $ cppBin bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.named "x" $ T.num8 $ toInteger x
              yv <- T.named "y" $ T.num8 $ toInteger y
              smtRes <- T.named "smtRes" $ (cppBinOpToFunc bop) xv yv
              cppResV <- T.named "cppRes" $ T.num8 $ toInteger (cppRes :: Int8)
              ok <- D.iseq (T.vnode smtRes) (T.vnode cppResV)
              D.named "ok" ok
              T.runSolver
          let (Just ok) = M.lookup "ok" vars
          Q.assert $ ok == 1

cppCmpI8OpTest bop = benchTestProperty ("QuickCheck " ++ show bop) cppT
  where cppT :: Int8 -> Int8 -> Q.Property
        cppT x y = Q.monadicIO $ do
          cppRes <- Q.run $ cppBin bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.num8 $ toInteger x
              yv <- T.num8 $ toInteger y
              T.named "result" $ (cppBinOpToFunc bop) xv yv
              T.runSolver
          let (Just smtRes) = M.lookup "result" vars
          Q.assert $ (smtRes == 1) == cppRes


cppUniI8OpTest bop = benchTestProperty ("QuickCheck " ++ show bop) cppT
  where cppT :: Int8 -> Int8 -> Q.Property
        cppT x y = Q.monadicIO $ do
          cppRes <- Q.run $ cppBin bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.named "x" $ T.num8 $ toInteger x
              smtRes <- T.named "smtRes" $ (cppUniOpToFunc bop) xv
              cppResV <- T.named "cppRes" $ T.num8 $ toInteger (cppRes :: Int8)
              ok <- D.iseq (T.vnode smtRes) (T.vnode cppResV)
              D.named "ok" ok
              T.runSolver
          let (Just ok) = M.lookup "ok" vars
          Q.assert $ ok == 1


cppBinW32OpTest bop = benchTestProperty ("QuickCheck " ++ show bop) cppT
  where cppT :: Word32 -> Word32 -> Q.Property
        cppT x y = Q.monadicIO $ do
          cppRes <- Q.run $ cppBin bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.named "x" $ T.unum $ toInteger x
              yv <- T.named "y" $ T.unum $ toInteger y
              smtRes <- T.named "smtRes" $ (cppBinOpToFunc bop) xv yv
              cppResV <- T.named "cppRes" $ T.unum $ toInteger (cppRes :: Word32)
              ok <- D.iseq (T.vnode smtRes) (T.vnode cppResV)
              D.named "ok" ok
              T.runSolver
          let (Just ok) = M.lookup "ok" vars
          Q.assert $ ok == 1

cppCmpW32OpTest bop = benchTestProperty ("QuickCheck " ++ show bop) cppT
  where cppT :: Word32 -> Word32 -> Q.Property
        cppT x y = Q.monadicIO $ do
          cppRes <- Q.run $ cppBin bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.unum $ toInteger x
              yv <- T.unum $ toInteger y
              T.named "result" $ (cppBinOpToFunc bop) xv yv
              T.runSolver
          let (Just smtRes) = M.lookup "result" vars
          Q.assert $ (smtRes == 1) == cppRes


cppUniW32OpTest bop = benchTestProperty ("QuickCheck " ++ show bop) cppT
  where cppT :: Word32 -> Word32 -> Q.Property
        cppT x y = Q.monadicIO $ do
          cppRes <- Q.run $ cppBin bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.named "x" $ T.unum $ toInteger x
              smtRes <- T.named "smtRes" $ (cppUniOpToFunc bop) xv
              cppResV <- T.named "cppRes" $ T.unum $ toInteger (cppRes :: Word32)
              ok <- D.iseq (T.vnode smtRes) (T.vnode cppResV)
              D.named "ok" ok
              T.runSolver
          let (Just ok) = M.lookup "ok" vars
          Q.assert $ ok == 1

cppBinW16OpTest bop = benchTestProperty ("QuickCheck " ++ show bop) cppT
  where cppT :: Word16 -> Word16 -> Q.Property
        cppT x y = Q.monadicIO $ do
          cppRes <- Q.run $ cppBin bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.named "x" $ T.unum16 $ toInteger x
              yv <- T.named "y" $ T.unum16 $ toInteger y
              smtRes <- T.named "smtRes" $ (cppBinOpToFunc bop) xv yv
              cppResV <- T.named "cppRes" $ T.unum16 $ toInteger (cppRes :: Word16)
              ok <- D.iseq (T.vnode smtRes) (T.vnode cppResV)
              D.named "ok" ok
              T.runSolver
          let (Just ok) = M.lookup "ok" vars
          Q.assert $ ok == 1

cppCmpW16OpTest bop = benchTestProperty ("QuickCheck " ++ show bop) cppT
  where cppT :: Word16 -> Word16 -> Q.Property
        cppT x y = Q.monadicIO $ do
          cppRes <- Q.run $ cppBin bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.unum16 $ toInteger x
              yv <- T.unum16 $ toInteger y
              T.named "result" $ (cppBinOpToFunc bop) xv yv
              T.runSolver
          let (Just smtRes) = M.lookup "result" vars
          Q.assert $ (smtRes == 1) == cppRes


cppUniW16OpTest bop = benchTestProperty ("QuickCheck " ++ show bop) cppT
  where cppT :: Word16 -> Word16 -> Q.Property
        cppT x y = Q.monadicIO $ do
          cppRes <- Q.run $ cppBin bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.named "x" $ T.unum16 $ toInteger x
              smtRes <- T.named "smtRes" $ (cppUniOpToFunc bop) xv
              cppResV <- T.named "cppRes" $ T.unum16 $ toInteger (cppRes :: Word16)
              ok <- D.iseq (T.vnode smtRes) (T.vnode cppResV)
              D.named "ok" ok
              T.runSolver
          let (Just ok) = M.lookup "ok" vars
          Q.assert $ ok == 1

cppBinW8OpTest bop = benchTestProperty ("QuickCheck " ++ show bop) cppT
  where cppT :: Word8 -> Word8 -> Q.Property
        cppT x y = Q.monadicIO $ do
          cppRes <- Q.run $ cppBin bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.named "x" $ T.unum8 $ toInteger x
              yv <- T.named "y" $ T.unum8 $ toInteger y
              smtRes <- T.named "smtRes" $ (cppBinOpToFunc bop) xv yv
              cppResV <- T.named "cppRes" $ T.unum8 $ toInteger (cppRes :: Word8)
              ok <- D.iseq (T.vnode smtRes) (T.vnode cppResV)
              D.named "ok" ok
              T.runSolver
          let (Just ok) = M.lookup "ok" vars
          Q.assert $ ok == 1

cppCmpW8OpTest bop = benchTestProperty ("QuickCheck " ++ show bop) cppT
  where cppT :: Word8 -> Word8 -> Q.Property
        cppT x y = Q.monadicIO $ do
          cppRes <- Q.run $ cppBin bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.unum8 $ toInteger x
              yv <- T.unum8 $ toInteger y
              T.named "result" $ (cppBinOpToFunc bop) xv yv
              T.runSolver
          let (Just smtRes) = M.lookup "result" vars
          Q.assert $ (smtRes == 1) == cppRes


cppUniW8OpTest bop = benchTestProperty ("QuickCheck " ++ show bop) cppT
  where cppT :: Word8 -> Word8 -> Q.Property
        cppT x y = Q.monadicIO $ do
          cppRes <- Q.run $ cppBin bop (x, y)
          (T.SolverSat vars) <- Q.run $ T.evalVerif Nothing $ do
              xv <- T.named "x" $ T.unum8 $ toInteger x
              smtRes <- T.named "smtRes" $ (cppUniOpToFunc bop) xv
              cppResV <- T.named "cppRes" $ T.unum8 $ toInteger (cppRes :: Word8)
              ok <- D.iseq (T.vnode smtRes) (T.vnode cppResV)
              D.named "ok" ok
              T.runSolver
          let (Just ok) = M.lookup "ok" vars
          Q.assert $ ok == 1


cppBinOpToFunc :: CppOp -> (T.VNode -> T.VNode -> D.Verif T.VNode)
cppBinOpToFunc op = case op of
  CppAdd -> T.cppAdd
  CppSub -> T.cppSub
  CppMul -> T.cppMul
  CppAnd -> T.cppAnd
  CppOr  -> T.cppOr
  CppXor -> T.cppXor
  CppShl -> T.cppShiftLeft
  CppShr -> T.cppShiftRight
  CppMin -> T.cppMin
  CppMax -> T.cppMax
  CppGt  -> T.cppGt
  CppGte -> T.cppGte
  CppLt  -> T.cppLt
  CppLte -> T.cppLte


cppUniOpToFunc :: CppOp -> (T.VNode -> D.Verif T.VNode)
cppUniOpToFunc op = case op of
  CppAbs -> T.cppAbs
  CppNeg -> T.cppNeg
  CppNot -> T.cppNot
