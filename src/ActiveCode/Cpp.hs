{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ActiveCode.Cpp {-(cpp, CPPOp(..)) -}where

import           ActiveCode.Utils
import           Control.Exception
import           Control.Monad

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
                        , "int main(int argc, char *argv[]) {"
                        , mainBody
                        , "return 0;"
                        , "}" ]

class Cpp a b where
  cppBin :: CppOp -> (a, a) -> IO b
  -- cppUni :: CppOp a -> a -> IO a

instance Cpp Double Double where
   cppBin op (x, y) = do
    cpp $ "printf(\"%g\", " ++ bop2code op x y ++ ");"



data CppOp = CppAdd
           | CppSub
           | CppMul
           --
           | CppAnd
           | CppOr
           | CppXor
           | CppNot
           | CppShl
           | CppShr
           --
           | CppMin
           | CppMax
           --
           | CppAbs
           | CppFloor
           | CppCeil
           --
           | CppGt
           | CppGte
           | CppLt
           | CppLte
           --
           | CppCast
           deriving (Eq, Show)

bop2code :: (Show a, Show b) => CppOp -> (a -> b -> String)
bop2code op = \x y -> case op of
  CppAdd  -> "(" ++ show x ++ ")" ++ "+"   ++ "(" ++ show y ++ ")"
  CppSub  -> "(" ++ show x ++ ")" ++ "-"   ++ "(" ++ show y ++ ")"
  CppMul  -> "(" ++ show x ++ ")" ++ "*"   ++ "(" ++ show y ++ ")"
  CppAnd  -> "(" ++ show x ++ ")" ++ "&"   ++ "(" ++ show y ++ ")"
  CppOr   -> "(" ++ show x ++ ")" ++ "|"   ++ "(" ++ show y ++ ")"
  CppXor  -> "(" ++ show x ++ ")" ++ "^"   ++ "(" ++ show y ++ ")"
  CppShl  -> "(" ++ show x ++ ")" ++ "<<"  ++ "(" ++ show y ++ ")"
  CppShr  -> "(" ++ show x ++ ")" ++ ">>"  ++ "(" ++ show y ++ ")"
  CppMin  -> "Min(" ++ show x ++ "," ++ show y ++ ")"
  CppMax  -> "Max(" ++ show x ++ "," ++ show y ++ ")"
  _      -> error "BUG: called bop2code with unary op"
