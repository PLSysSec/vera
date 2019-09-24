{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DSL.DSL ( i64
               , i32
               , i16
               , i8
               , i1
               , i64c
               , i64max
               , i64min
               , i32c
               , i32max
               , ui32max
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
               , true
               , false
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
               , named
               , assign
               , conjunction
               , disjunction
               , jsSll32
               , jsSrl32
               , jsSra32
               , module DSL.BoolectorWrapper
               -- ** Verif monad
               , Verif
               , getVars
               , VerifState(..)
               , runVerif
               , evalVerif
               , execVerif
               ) where
import           Control.Monad              (foldM)
import           Control.Monad.State.Strict
import qualified Data.Map.Strict            as M
import           DSL.BoolectorWrapper       hiding (false, true)
import qualified DSL.BoolectorWrapper       as B
import           Prelude                    hiding (max, min)

-- | Verification state. I'm assuming we'll eventually need
-- to keep track of more things
data VerifState = VerifState { vars :: M.Map String B.Node }

newtype Verif a = Verif (StateT VerifState B.Boolector a)
    deriving (Functor, Applicative, Monad, MonadState VerifState, MonadIO)

instance B.MonadBoolector Verif where
    getBoolectorState = Verif $ lift $ get
    putBoolectorState state = Verif $ lift $ put state

getVars :: Verif (M.Map String B.Node)
getVars = vars `liftM` get

emptyVerifState :: VerifState
emptyVerifState = VerifState { vars = M.empty }

-- | Run verification computation
runVerif :: Maybe Integer -- ^ Optional timeout
         -> Verif a       -- ^ Verification computation
         -> IO (a, VerifState)
runVerif mTimeout (Verif act) = do
  bs <- B.newBoolectorState mTimeout
  B.evalBoolector bs $ runStateT act emptyVerifState

evalVerif :: Maybe Integer -> Verif a -> IO a
evalVerif mt act = fst <$> runVerif mt act

execVerif :: Maybe Integer -> Verif a -> IO VerifState
execVerif mt act = snd <$> runVerif mt act

-- Standard sorts

i64 :: Verif B.Sort
i64 = B.bitvecSort 64

i32 :: Verif B.Sort
i32 = B.bitvecSort 32

i16 :: Verif B.Sort
i16 = B.bitvecSort 16

i8 :: Verif B.Sort
i8 = B.bitvecSort 8

i1 :: Verif B.Sort
i1 = B.bitvecSort 1

-- Integer consts

-- | 64-bit constant
i64c :: Integer -> Verif B.Node
i64c val | val <= 18446744073709551615 = i64 >>= B.unsignedInt val
         | otherwise = error $ unwords $ [show val, "is past the range of i64s"]

i64max :: Verif B.Node
i64max = i64c 9223372036854775807

i64min :: Verif B.Node
i64min = undefined

-- | 32-bit constant
i32c :: Integer -> Verif B.Node
i32c val | val <= 4294967295 = i32 >>= B.unsignedInt val
         | otherwise = error $ unwords $ [show val, "is past the range of i32s"]

ui32max :: Verif B.Node
ui32max = i32c 4294967295

i32max :: Verif B.Node
i32max = i32c 2147483647

i32min :: Verif B.Node
i32min = i32c 2147483648

-- | 16-bit constant
i16c :: Integer -> Verif B.Node
i16c val | val <= 65535 = i16 >>= B.unsignedInt val
         | otherwise = error $ unwords $ [show val, "is past the range of i16s"]

i16max :: Verif B.Node
i16max = undefined

i16min :: Verif B.Node
i16min = undefined

-- | 8-bit constant
i8c :: Integer -> Verif B.Node
i8c val | val <= 255 = i8 >>= B.unsignedInt val
        | otherwise = error $ unwords $ [show val, "is past the range of i8s"]

i8max :: Verif B.Node
i8max = undefined

i8min :: Verif B.Node
i8min = undefined

-- | 1-bit constant
i1c :: Integer -> Verif B.Node
i1c val | val <= 1 = i1 >>= B.unsignedInt val
        | otherwise = error $ unwords $ [show val, "is past the range of i1s"]

i1max :: Verif B.Node
i1max = undefined

i1min :: Verif B.Node
i1min = undefined

true :: Verif B.Node
true = i1c 1

false :: Verif B.Node
false = i1c 0

-- Integer variables

var' :: Verif B.Sort -> String -> Verif B.Node
var' sort name = do
  s0 <- get
  let allVars = vars s0
  case M.lookup name allVars of
    Just _ -> error $ unwords ["Already created variable named", name]
    _ -> do
      sort' <- sort
      result <- B.var sort' name
      put $ s0 { vars = M.insert name result allVars }
      return result

i64v :: String -> Verif B.Node
i64v name = var' i64 name

i32v :: String -> Verif B.Node
i32v name = var' i32 name

i16v :: String -> Verif B.Node
i16v name = var' i16 name

i8v :: String -> Verif B.Node
i8v name = var' i8 name

i1v :: String -> Verif B.Node
i1v name = var' i1 name

-- | Named intermediate expression
named :: String -> Verif B.Node -> Verif B.Node
named str act = do
  res <- act
  v <- var' (B.getSort res) str
  assign v res
  return v

-- Functions

smin :: B.Node -> B.Node -> Verif B.Node
smin x y = do
  isLess <- B.slte x y
  B.cond isLess x y

smax :: B.Node -> B.Node -> Verif B.Node
smax x y = do
  isMore <- B.sgte x y
  B.cond isMore x y

umin :: B.Node -> B.Node -> Verif B.Node
umin x y = do
  isLess <- B.ulte x y
  B.cond isLess x y

umax :: B.Node -> B.Node -> Verif B.Node
umax x y = do
  isMore <- B.ugte x y
  B.cond isMore x y

assign :: B.Node -> B.Node -> Verif ()
assign x y = B.eq x y >>= B.assert

conjunction :: [B.Node] -> Verif B.Node
conjunction [] = error "Cannot have a conjunction of zero nodes"
conjunction xs = foldM B.and (head xs) (tail xs)

disjunction :: [B.Node] -> Verif B.Node
disjunction [] = error "Cannot have a disjunction of zero nodes"
disjunction xs = foldM B.or (head xs) (tail xs)

-- | https://www.ecma-international.org/ecma-262/5.1/#sec-11.7.3
jsShiftWrapper :: (B.Node -> B.Node -> Verif B.Node)
               -> B.Node
               -> B.Node
               -> Verif B.Node
jsShiftWrapper op left right = do
  maskedRight <- i32c 31 >>= B.and right
  op left maskedRight

jsSll32, jsSrl32, jsSra32 :: B.Node -> B.Node -> Verif B.Node
jsSll32 = jsShiftWrapper B.safeSll
jsSrl32 = jsShiftWrapper B.safeSrl
jsSra32 = jsShiftWrapper B.safeSra
