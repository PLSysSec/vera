module DSL ( i64
           , i32
           , i16
           , i8
           , i1
           , i64c
           , i64max
           , i64min
           , i32c
           , i32max
           , i32min
           , i16c
           , i16max
           , i16min
           , i8c
           , i8max
           , i8min
           , i1c
           , i1max
           , i1min
           , i64v
           , i32v
           , i16v
           , i8v
           , i1v
           , smin
           , smax
           , umin
           , umax
           , var'
           , module BoolectorWrapper
           ) where
import           BoolectorWrapper
import qualified BoolectorWrapper as B
import           Prelude          hiding (max, min)

-- Standard sorts

i64 :: (B.MonadBoolector m) => m B.Sort
i64 = B.bitvecSort 64

i32 :: (B.MonadBoolector m) => m B.Sort
i32 = B.bitvecSort 32

i16 :: (B.MonadBoolector m) => m B.Sort
i16 = B.bitvecSort 16

i8 :: (B.MonadBoolector m) => m B.Sort
i8 = B.bitvecSort 8

i1 :: (B.MonadBoolector m) => m B.Sort
i1 = B.bitvecSort 1

-- Integer consts

-- | 64-bit constant
i64c :: (B.MonadBoolector m) => Integer -> m B.Node
i64c val | val <= 18446744073709551615 = i64 >>= B.unsignedInt val
        | otherwise = error $ unwords $ [show val, "is past the range of i64s"]

i64max :: (B.MonadBoolector m) => m B.Node
i64max = i64c 9223372036854775807

i64min :: (B.MonadBoolector m) => m B.Node
i64min = undefined

-- | 32-bit constant
i32c :: (B.MonadBoolector m) => Integer -> m B.Node
i32c val | val <= 4294967295 = i32 >>= B.unsignedInt val
        | otherwise = error $ unwords $ [show val, "is past the range of i32s"]

i32max :: (B.MonadBoolector m) => m B.Node
i32max = i32c 2147483647

i32min :: (B.MonadBoolector m) => m B.Node
i32min = i32c 2147483648

-- | 16-bit constant
i16c :: (B.MonadBoolector m) => Integer -> m B.Node
i16c val | val <= 65535 = i16 >>= B.unsignedInt val
        | otherwise = error $ unwords $ [show val, "is past the range of i16s"]

i16max :: (B.MonadBoolector m) => m B.Node
i16max = undefined

i16min :: (B.MonadBoolector m) => m B.Node
i16min = undefined

-- | 8-bit constant
i8c :: (B.MonadBoolector m) => Integer -> m B.Node
i8c val | val <= 255 = i8 >>= B.unsignedInt val
       | otherwise = error $ unwords $ [show val, "is past the range of i8s"]

i8max :: (B.MonadBoolector m) => m B.Node
i8max = undefined

i8min :: (B.MonadBoolector m) => m B.Node
i8min = undefined

-- | 1-bit constant
i1c :: (B.MonadBoolector m) => Integer -> m B.Node
i1c val | val <= 1 = i1 >>= B.unsignedInt val
       | otherwise = error $ unwords $ [show val, "is past the range of i1s"]

i1max :: (B.MonadBoolector m) => m B.Node
i1max = undefined

i1min :: (B.MonadBoolector m) => m B.Node
i1min = undefined

-- Integer variables

var' :: (B.MonadBoolector m) => m B.Sort -> String -> m B.Node
var' sort name = sort >>= \s -> B.var s name

i64v :: (B.MonadBoolector m) => String -> m B.Node
i64v name = var' i64 name

i32v :: (B.MonadBoolector m) => String -> m B.Node
i32v name = var' i32 name

i16v :: (B.MonadBoolector m) => String -> m B.Node
i16v name = var' i16 name

i8v :: (B.MonadBoolector m) => String -> m B.Node
i8v name = var' i8 name

i1v :: (B.MonadBoolector m) => String -> m B.Node
i1v name = var' i1 name

-- Functions

smin :: (B.MonadBoolector m) => B.Node -> B.Node -> m B.Node
smin x y = do
  isLess <- B.slte x y
  B.cond isLess x y

smax :: (B.MonadBoolector m) => B.Node -> B.Node -> m B.Node
smax x y = do
  isMore <- B.sgte x y
  B.cond isMore x y

umin :: (B.MonadBoolector m) => B.Node -> B.Node -> m B.Node
umin x y = do
  isLess <- B.ulte x y
  B.cond isLess x y

umax :: (B.MonadBoolector m) => B.Node -> B.Node -> m B.Node
umax x y = do
  isMore <- B.ugte x y
  B.cond isMore x y







