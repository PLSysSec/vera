{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ActiveCode.Cpp {-(cpp, CPPOp(..)) -}where

import           ActiveCode.Utils
import           Control.Exception
import           Control.Monad
import           Data.Binary.IEEE754

import           System.FilePath
import           System.Posix.Temp
import           System.Directory (removeFile)
import           System.Exit
import           System.IO

cpp :: Read a => String -> IO a
cpp mainBody = do
  fp <- bracket (mkstemps "/tmp/activeC" ".cpp")
                (hClose . snd)
                (\(f,h) -> hPutStr h src >> return (dropExtension f))

  -- compile
  (ccode,cout) <- readCommand cc ["-o", fp, fp ++ ".cpp"] ""
  -- removeFile $ fp ++ ".cpp"
  unless (ccode == ExitSuccess) $ fail cout
  -- run
  (code,out) <- readCommand fp [] ""
  -- removeFile fp
  unless (code == ExitSuccess) $ fail out
  readIO out

    where cc = "c++"
          src = unlines [ "#include <stdio.h>"
                        , "#include <cmath>"
                        , "#include <stdint.h>"
                        , ""
                        , "template <class T>"
                        , "static constexpr inline T Min(T t1, T t2) {"
                        , "  return t1 < t2 ? t1 : t2;"
                        , "}"
                        , ""
                        , "template <class T>"
                        , "static constexpr inline T Max(T t1, T t2) {"
                        , "  return t1 > t2 ? t1 : t2;"
                        , "}"
                        , ""
                        , "namespace detail {"
                        , ""
                        , "// For now mozilla::Abs only takes intN_T, the signed natural types, and"
                        , "// float/double/long double.  Feel free to add overloads for other standard,"
                        , "// signed types if you need them."
                        , ""
                        , "template <typename T>"
                        , "struct AbsReturnTypeFixed;"
                        , ""
                        , "template <>"
                        , "struct AbsReturnTypeFixed<int8_t> {"
                        , "  typedef uint8_t Type;"
                        , "};"
                        , "template <>"
                        , "struct AbsReturnTypeFixed<int16_t> {"
                        , "  typedef uint16_t Type;"
                        , "};"
                        , "template <>"
                        , "struct AbsReturnTypeFixed<int32_t> {"
                        , "  typedef uint32_t Type;"
                        , "};"
                        , "template <>"
                        , "struct AbsReturnTypeFixed<int64_t> {"
                        , "  typedef uint64_t Type;"
                        , "};"
                        , ""
                        , "template <typename T>"
                        , "struct AbsReturnType : AbsReturnTypeFixed<T> {};"
                        , ""
                        , "template <>"
                        , "struct AbsReturnType<short> {"
                        , "  typedef unsigned short Type;"
                        , "};"
                        , "template <>"
                        , "struct AbsReturnType<int> {"
                        , "  typedef unsigned int Type;"
                        , "};"
                        , "template <>"
                        , "struct AbsReturnType<long> {"
                        , "  typedef unsigned long Type;"
                        , "};"
                        , "template <>"
                        , "struct AbsReturnType<long long> {"
                        , "  typedef unsigned long long Type;"
                        , "};"
                        , "template <>"
                        , "struct AbsReturnType<float> {"
                        , "  typedef float Type;"
                        , "};"
                        , "template <>"
                        , "struct AbsReturnType<double> {"
                        , "  typedef double Type;"
                        , "};"
                        , "template <>"
                        , "struct AbsReturnType<long double> {"
                        , "  typedef long double Type;"
                        , "};"
                        , ""
                        , "}  // namespace detail"
                        , "template <typename T>"
                        , "inline constexpr typename detail::AbsReturnType<T>::Type Abs(const T aValue) {"
                        , "  using ReturnType = typename detail::AbsReturnType<T>::Type;"
                        , "  return aValue >= 0 ? ReturnType(aValue) : ~ReturnType(aValue) + 1;"
                        , "}"
                        , ""
                        , "template <>"
                        , "inline float Abs<float>(const float aFloat) {"
                        , "  return std::fabs(aFloat);"
                        , "}"
                        , ""
                        , "template <>"
                        , "inline double Abs<double>(const double aDouble) {"
                        , "  return std::fabs(aDouble);"
                        , "}"
                        , ""
                        , "template <>"
                        , "inline long double Abs<long double>(const long double aLongDouble) {"
                        , "  return std::fabs(aLongDouble);"
                        , "}"
                        , ""
                        , "int main(int argc, char *argv[]) {"
                        , mainBody
                        , "return 0;"
                        , "}" ]

class Cpp a b where
  cppBin :: CppOp -> (a, a) -> IO b
  cppUni :: CppOp -> a -> IO b

instance Cpp Double Double where
   cppBin op (x, y) = do
    i <- cpp $ unlines [ "double x = " ++ show x ++ ";"
                       , "double y = " ++ show y ++ ";"
                       , "double result = " ++ op2code op ++ ";"
                       , "printf(\"%ld\", *(uint64_t*)(&result));"
                       ]
    return $ wordToDouble $ fromInteger i
   cppUni op x = do
    i <- cpp $ unlines [ "double x = " ++ show x ++ ";"
                       , "double result = " ++ op2code op ++ ";"
                       , "printf(\"%ld\", *(uint64_t*)(&result));"
                       ]
    return $ wordToDouble $ fromInteger i



data CppOp = CppAdd
           | CppSub
           | CppMul
           --
           | CppAnd
           | CppOr
           | CppXor
           | CppNeg
           | CppNot
           | CppShl
           | CppShr
           --
           | CppMin
           | CppMax
           --
           | CppAbs
           --
           | CppGt
           | CppGte
           | CppLt
           | CppLte
           --
           | CppCast
           deriving (Eq, Show)

op2code :: CppOp -> String
op2code op = case op of
  CppAdd   -> "(x +  y)"
  CppSub   -> "(x -  y)"
  CppMul   -> "(x *  y)"
  CppAnd   -> "(x &  y)"
  CppOr    -> "(x |  y)"
  CppXor   -> "(x ^  y)"
  CppShl   -> "(x << y)"
  CppShr   -> "(x >> y)"
  CppMin   -> "Min(x, y)"
  CppMax   -> "Max(x, y)"
  CppGt    -> "(x > y)"
  CppGte   -> "(x >= y)"
  CppLt    -> "(x < y)"
  CppLte   -> "(x <= y)"
  CppNeg   -> "(-x)"
  CppNot   -> "(~x)"
  CppAbs   -> "Abs(x)"
  _      -> error "BUG: called op2code with bad op"
