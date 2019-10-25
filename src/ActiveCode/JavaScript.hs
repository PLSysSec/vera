{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ActiveCode.JavaScript (js, JSOp(..)) where

import           ActiveCode.Utils
import           Data.Int
import           Data.Word
import           Control.Monad
import           System.Exit

class JS a b where
  js :: JSOp -> a -> IO b

instance JS (Double, Double) Double where
  js = jsBin
instance JS (Int32, Word32) Int32 where
  js = jsBin
instance JS (Word32, Word32) Word32 where
  js = jsBin
instance JS Double Double where
  js = jsUni
instance JS Int32 Int32 where
  js = jsUni

jsBin :: (Show a, Show b, Read c) => JSOp -> (a, b) -> IO c
jsBin op (x, y) = do
    (code,out) <- readCommand "node" [] $ "console.log(" ++ bop2code op x y ++ ")"
    unless (code == ExitSuccess) $ fail "failed"
    readIO out

jsUni :: (Show a, Read b) => JSOp -> a -> IO b
jsUni op x = do
    (code,out) <- readCommand "node" [] $ "console.log(" ++ uop2code op x ++ ")"
    unless (code == ExitSuccess) $ fail "failed"
    readIO out



data JSOp = JSAdd
          | JSSub
          | JSAnd
          | JSOr
          | JSXor
          | JSNot
          | JSMul
          | JSShl
          | JSShr
          | JSUshr
          | JSAbs
          | JSMin
          | JSMax
          | JSFloor
          | JSCeil
          | JSSign
          deriving (Eq, Show)

bop2code :: (Show a, Show b) => JSOp -> (a -> b -> String)
bop2code op = \x y -> case op of
  JSAdd  -> "(" ++ show x ++ ")" ++ "+"   ++ "(" ++ show y ++ ")"
  JSSub  -> "(" ++ show x ++ ")" ++ "-"   ++ "(" ++ show y ++ ")"
  JSAnd  -> "(" ++ show x ++ ")" ++ "&"   ++ "(" ++ show y ++ ")"
  JSOr   -> "(" ++ show x ++ ")" ++ "|"   ++ "(" ++ show y ++ ")"
  JSXor  -> "(" ++ show x ++ ")" ++ "^"   ++ "(" ++ show y ++ ")"
  JSMul  -> "(" ++ show x ++ ")" ++ "*"   ++ "(" ++ show y ++ ")"
  JSShl  -> "(" ++ show x ++ ")" ++ "<<"  ++ "(" ++ show y ++ ")"
  JSShr  -> "(" ++ show x ++ ")" ++ ">>"  ++ "(" ++ show y ++ ")"
  JSUshr -> "(" ++ show x ++ ")" ++ ">>>" ++ "(" ++ show y ++ ")"
  JSMin  -> "Math.min(" ++ show x ++ "," ++ show y ++ ")"
  JSMax  -> "Math.max(" ++ show x ++ "," ++ show y ++ ")"
  _      -> error "BUG: called bop2code with unary op"
  
  
  
  


uop2code :: Show a => JSOp -> (a -> String)
uop2code op = \x -> case op of
  JSNot   -> "~" ++ show x
  JSAbs   -> "Math.abs("   ++ show x ++ ")"
  JSFloor -> "Math.floor(" ++ show x ++ ")"
  JSCeil  -> "Math.ceil(" ++ show x ++ ")"
  JSSign  -> "Math.sign(" ++ show x ++ ")"
  _       -> error "BUG: called uop2code with binary op"
