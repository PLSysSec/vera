module DSL where
import qualified Wrapper as B



-- Integers constants

-- | 64-bit constant
i64 :: (B.MonadBoolector m) => Integer -> m B.Node
i64 val | val <= 18446744073709551615 = B.bitvecSort 64 >>= B.unsignedInt val
        | otherwise = error $ unwords $ [show val, "is past the range of i64s"]

-- | 32-bit constant
i32 :: (B.MonadBoolector m) => Integer -> m B.Node
i32 val | val <= 4294967295 = B.bitvecSort 32 >>= B.unsignedInt val
        | otherwise = error $ unwords $ [show val, "is past the range of i32s"]

-- | 16-bit constant
i16 :: (B.MonadBoolector m) => Integer -> m B.Node
i16 val | val <= 65535 = B.bitvecSort 16 >>= B.unsignedInt val
        | otherwise = error $ unwords $ [show val, "is past the range of i16s"]

-- | 8-bit constant
i8 :: (B.MonadBoolector m) => Integer -> m B.Node
i8 val | val <= 255 = B.bitvecSort 8 >>= B.unsignedInt val
       | otherwise = error $ unwords $ [show val, "is past the range of i8s"]

-- | 1-bit constant
i1 :: (B.MonadBoolector m) => Integer -> m B.Node
i1 val | val <= 1 = B.bitvecSort 1 >>= B.unsignedInt val
       | otherwise = error $ unwords $ [show val, "is past the range of i1s"]

-- Floating point constants




