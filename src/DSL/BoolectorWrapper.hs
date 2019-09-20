module DSL.BoolectorWrapper ( safeSll
                            , safeSrl
                            , safeSra
                            , safeRol
                            , safeRor
                            , castToWidth
                            , module Boolector
                            ) where
import           Boolector                  hiding (apply, exists, rol, ror,
                                             sll, sra, srl, uf)
import qualified Boolector                  as B
import           Control.Monad.State.Strict (unless)
import           Data.Word                  (Word32)

-- | Safe boolector shift operations
--
-- Boolector puts restrictions on the widths of the arguments to shift operations.
-- As of Boolector 3, the widths of both operands must be the same.
-- As of Boolectors < 3, the width of the first operand had to be a power of 2,
-- and the width of the second operand had to be log 2 of the first.
-- We DO NOT support the Boolector < 3 restriction
safeSll, safeSrl, safeSra, safeRol, safeRor :: MonadBoolector m => Node -> Node -> m Node
safeSll = shiftWrapper B.sll
safeSrl = shiftWrapper B.srl
safeSra = shiftWrapper B.sra
safeRol = shiftWrapper B.rol
safeRor = shiftWrapper B.ror

-- | Wrapper for boolector shift operations
shiftWrapper :: (MonadBoolector m)
             => (Node -> Node -> m Node) -- ^ Shift op
             -> Node -- ^ Thing to shift
             -> Node -- ^ Thing to shift by
             -> m Node -- ^ Result
shiftWrapper shiftOp toShift shiftVal = do
  castVal <- getWidth toShift >>= castToWidth shiftVal
  shiftOp toShift castVal

-- | Cast a node to a new width
-- If the new width is larger, use unsigned extension
-- If the new width is smaller, slice
castToWidth :: (MonadBoolector m)
            => Node -- ^ Node to be cast
            -> Word32 -- ^ New width
            -> m Node -- ^ Result
castToWidth varToCast newWidth = do
  sort <- getSort varToCast
  let isBv = isBitvecSort sort || isBoolSort sort
  unless isBv $ error $ "Should never cast non-bitvector sort (" ++ show sort ++ ")"
  oldWidth' <- getWidth varToCast
  let oldWidth = fromIntegral oldWidth'
  case compare oldWidth newWidth of
    LT -> uext varToCast (newWidth - oldWidth)
    GT -> slice varToCast (newWidth - 1) 0
    _  -> return varToCast

