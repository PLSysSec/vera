module DSL where
import           Prelude hiding (max, min)
import qualified Wrapper as B

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

-- | 32-bit constant
i32c :: (B.MonadBoolector m) => Integer -> m B.Node
i32c val | val <= 4294967295 = i32 >>= B.unsignedInt val
        | otherwise = error $ unwords $ [show val, "is past the range of i32s"]

-- | 16-bit constant
i16c :: (B.MonadBoolector m) => Integer -> m B.Node
i16c val | val <= 65535 = i16 >>= B.unsignedInt val
        | otherwise = error $ unwords $ [show val, "is past the range of i16s"]

-- | 8-bit constant
i8c :: (B.MonadBoolector m) => Integer -> m B.Node
i8c val | val <= 255 = i8 >>= B.unsignedInt val
       | otherwise = error $ unwords $ [show val, "is past the range of i8s"]

-- | 1-bit constant
i1c :: (B.MonadBoolector m) => Integer -> m B.Node
i1c val | val <= 1 = i1 >>= B.unsignedInt val
       | otherwise = error $ unwords $ [show val, "is past the range of i1s"]

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





