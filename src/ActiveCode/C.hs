{-# LANGUAGE OverloadedStrings #-}
module ActiveCode.C (c,cpp) where

import qualified Data.ByteString.Lazy.Char8 as L8

import           ActiveCode.Utils
import           Control.Exception
import           Control.Monad

import           System.FilePath
import           System.Posix.Temp
import           System.Process
import           System.Exit
import           System.IO (hClose)

import qualified Data.Text as T
import qualified Data.Text.IO as T

c :: L8.ByteString -> IO ()
c = clang "clang" ".c"

cpp :: L8.ByteString -> IO ()
cpp = clang "clang++" ".cpp"

clang :: String -> String -> L8.ByteString -> IO ()
clang cc ext src = do
  fp <- bracket (mkstemps "/tmp/activeC" ext)
                (hClose . snd)
                (\(f,h) -> L8.hPut h src >> return (dropExtension f))
  let rpl = T.replace (T.pack $ fp++ext) "<user-input>" 
  -- compile
  (ccode,cout') <- readCommand cc ["-o",fp,fp++ext] ""
  let cout = rpl $ T.pack cout'
  unless (ccode == ExitSuccess) $ T.putStr cout >> exitWith ccode
  -- run
  (code,out') <- readCommand fp [] ""
  let out = rpl $ T.pack out'
  T.putStr out
  exitWith code
