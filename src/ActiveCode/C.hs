{-# LANGUAGE OverloadedStrings #-}
module ActiveCode.C {-(cpp, CPPOp(..)) -}where

import           ActiveCode.Utils
import           Control.Exception
import           Control.Monad

import           System.FilePath
import           System.Posix.Temp
import           System.Directory (removeFile)
import           System.Exit
import           System.IO

cpp :: String -> IO ()
cpp mainBody = do
  fp <- bracket (mkstemps "/tmp/activeC" ".cpp")
                (hClose . snd)
                (\(f,h) -> hPutStr h src >> return (dropExtension f))

  -- compile
  (ccode,cout) <- readCommand cc ["-o", fp, fp ++ ".cpp"] ""
  removeFile $ fp ++ ".cpp"
  unless (ccode == ExitSuccess) $ fail cout
  -- run
  (code,out) <- readCommand fp [] ""
  removeFile fp
  unless (ccode == ExitSuccess) $ fail cout
  unless (code == ExitSuccess) $ fail out
  putStrLn out

    where cc = "cc"
          src = unlines [ "#include <stdio.h>"
                        , "#include <stdint.h>"
                        , "int main(int argc, char *argv[]) {"
                        , mainBody
                        , "return 0;"
                        , "}" ]
