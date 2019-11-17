{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DSL.DSL ( i64
               , i32
               , i16
               , i8
               , i1
               , getNextCt
               , significandConst
               , exponentConst
               , exponent
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
               , doubv
               , smin
               , smax
               , umin
               , umax
               , var'
               , named
               , assign
               , conjunction
               , disjunction
               , module DSL.Z3Wrapper
               -- ** Verif monad
               , Verif
               , VerifState(..)
               , SMTResult(..)
               , isSat
               , isUnsat
               , getVars
               , runVerif
               , evalVerif
               , execVerif
               , runSolver
               ) where
import           Control.Monad              (foldM)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Binary.IEEE754
import           Data.Bits
import           Data.Char                  (digitToInt)
import           Data.List                  (foldl')
import           Data.List.Split
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (catMaybes)
import           DSL.Z3Wrapper
import qualified DSL.Z3Wrapper              as Z3
import           Prelude                    hiding (exp, exponent, map, max,
                                             min, not)
import qualified Z3.Monad                   as Z

{-|

Low-level DSL for manipulating SMT variables.

-}

-- | Verification state. I'm assuming we'll eventually need
-- to keep track of more things
data VerifState = VerifState { vars         :: M.Map String Z3.Node
                             , solverResult :: SMTResult
                             , ctr          :: Int
                             }

isSat :: SMTResult -> Bool
isSat SolverSat{} = True
isSat _           = False

isUnsat :: SMTResult -> Bool
isUnsat SolverUnsat{} = True
isUnsat _             = False

newtype Verif a = Verif (StateT VerifState Z.Z3 a)
    deriving (Functor, Applicative, Monad, MonadState VerifState, MonadIO)

instance Z.MonadZ3 Verif where
    getSolver = Verif $ lift $ Z.getSolver
    getContext = Verif $ lift $ Z.getContext

data SMTResult = SolverSat { example :: (M.Map String Double) }
               | SolverUnsat
               | SolverFailed
               deriving (Eq, Ord, Show)

getVars :: Verif (M.Map String Z3.Node)
getVars = vars `liftM` get

emptyVerifState :: VerifState
emptyVerifState = VerifState { vars = M.empty
                             , solverResult = SolverFailed
                             , ctr = 0
                             }

-- | Run verification computation
runVerif :: Maybe Integer -- ^ Optional timeout
         -> Verif a       -- ^ Verification computation
         -> IO (a, VerifState)
runVerif mTimeout (Verif act) =
  -- Z.evalZ3 $ runStateT act emptyVerifState
  Z.evalZ3With Nothing (Z.opt "timeout" (5000 :: Int)) $ runStateT act emptyVerifState

evalVerif :: Maybe Integer -> Verif a -> IO a
evalVerif mt act = fst <$> runVerif mt act

execVerif :: Maybe Integer -> Verif a -> IO VerifState
execVerif mt act = snd <$> runVerif mt act

runSolver :: Verif SMTResult
runSolver = do
  z3result <- Z.solverCheck
  result <- case z3result of
    Z.Sat -> do
      model <- Z.solverGetModel
      strModel <- Z.modelToString model
      intModel <- liftIO $ getIntModel strModel
      return $ SolverSat intModel
    Z.Unsat -> return SolverUnsat
    _ -> return SolverFailed
  s0 <- get
  put $ s0 { solverResult = result }
  return result


getIntModel :: String -> IO (M.Map String Double)
getIntModel str = do
  let modelLines = splitOn "\n" str
  vs <- forM modelLines $ \line -> case splitOn "->" line of
            [var, strVal] -> do
              let maybeHexVal = drop 2 strVal
                  val = case maybeHexVal of
                          -- Negative 0
                          '_':' ':'-':'z':'e':'r':'o':_ -> Just (-0.0)
                          '_':' ':'+':'z':'e':'r':'o':_ -> Just (0.0)
                          '_':' ':'N':'a':'N':_         -> Just $ 0 / 0
                          '_':' ':'-':_                 -> Just $ negate $ 1 / 0
                          '_':' ':'+':_                 -> Just $ 1 / 0
                          -- Boolean
                          'b':n                         -> Just (read n :: Double)
                          -- Hex
                          'x':_                         -> Just (read ('0':maybeHexVal) :: Double)
                          'f':'p':' ':rest              ->
                            let components = splitOn " " rest
                                sign = read (drop 2 $ components !! 0) :: Integer
                                exp = toDec $ drop 2 $ components !! 1
                                sig = read ('0':(drop 1 $ init $ components !! 2)) :: Integer
                                result = (sig .&. 0xfffffffffffff) .|. ((exp .&. 0x7ff) `shiftL` 52) .|. ((sign .&. 0x1) `shiftL` 63)
                            in Just $ wordToDouble $ fromIntegral $ result
                          _                             -> Nothing
              return $ case val of
                   -- gross for printing
                   Just v  -> Just (init var, v)
                   Nothing -> Nothing
            _ -> return Nothing
  return $ M.fromList $ catMaybes vs
  where
    -- https://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell
    toDec :: String -> Integer
    toDec = foldl' (\acc x -> acc * 2 + (fromIntegral $ digitToInt x)) 0

getNextCt :: Verif Int
getNextCt = do
  s0 <- get
  let ct = ctr s0
  put $ s0 { ctr = ct + 1 }
  return ct

--
-- Ints and stuff
--

doub :: Verif Z3.Sort
doub = Z.mkDoubleSort

i64 :: Verif Z3.Sort
i64 = Z.mkBvSort 64

i32 :: Verif Z3.Sort
i32 = Z.mkBvSort 32

i16 :: Verif Z3.Sort
i16 = Z.mkBvSort 16

i8 :: Verif Z3.Sort
i8 = Z.mkBvSort 8

i1 :: Verif Z3.Sort
i1 = Z.mkBvSort 1

-- Integer consts

significandConst :: Integer -> Verif Z3.Node
significandConst val = Z.mkBitvector 52 val

exponentConst :: Integer -> Verif Z3.Node
exponentConst val = Z.mkBitvector 11 val

exponent :: Z3.Node -> Verif Z3.Node
exponent expVal = do
  _1023 <- Z.mkBitvector 11 1023
  Z3.sub expVal _1023

-- | 64-bit constant
i64c :: Integer -> Verif Z3.Node
i64c val | val <= 18446744073709551615 = Z.mkBitvector 64 val
         | otherwise = error $ unwords $ [show val, "is past the range of i64s"]

i64max :: Verif Z3.Node
i64max = i64c 9223372036854775807

i64min :: Verif Z3.Node
i64min = undefined

-- | 32-bit constant
i32c :: Integer -> Verif Z3.Node
i32c val | val <= 4294967295 = Z.mkBitvector 32 val
         | otherwise = error $ unwords $ [show val, "is past the range of i32s"]

ui32max :: Verif Z3.Node
ui32max = i32c 4294967295

i32max :: Verif Z3.Node
i32max = i32c 2147483647

i32min :: Verif Z3.Node
i32min = i32c (-2147483648)

-- | 16-bit constant
i16c :: Integer -> Verif Z3.Node
i16c val | val <= 65535 = Z.mkBitvector 16 val
         | otherwise = error $ unwords $ [show val, "is past the range of i16s"]

i16max :: Verif Z3.Node
i16max = undefined

i16min :: Verif Z3.Node
i16min = undefined

-- | 8-bit constant
i8c :: Integer -> Verif Z3.Node
i8c val | val <= 255 = Z.mkBitvector 8 val
        | otherwise = error $ unwords $ [show val, "is past the range of i8s"]

i8max :: Verif Z3.Node
i8max = undefined

i8min :: Verif Z3.Node
i8min = undefined

-- | 1-bit constant
i1c :: Integer -> Verif Z3.Node
i1c val | val <= 1 = Z.mkBitvector 1 val
        | otherwise = error $ unwords $ [show val, "is past the range of i1s"]

i1max :: Verif Z3.Node
i1max = undefined

i1min :: Verif Z3.Node
i1min = undefined

true :: Verif Z3.Node
true = i1c 1

false :: Verif Z3.Node
false = i1c 0

-- Integer variables

var' :: Verif Z3.Sort -> String -> Verif Z3.Node
var' sort name = do
  s0 <- get
  let allVars = vars s0
  case M.lookup name allVars of
    Just _ -> error $ unwords ["Already created variable named", name]
    _ -> do
      sort' <- sort
      sym <- Z.mkStringSymbol name
      result <- Z.mkVar sym sort'
      put $ s0 { vars = M.insert name result allVars }
      return result

doubv :: String -> Verif Z3.Node
doubv name = var' doub name

i64v :: String -> Verif Z3.Node
i64v name = var' i64 name

i32v :: String -> Verif Z3.Node
i32v name = var' i32 name

i16v :: String -> Verif Z3.Node
i16v name = var' i16 name

i8v :: String -> Verif Z3.Node
i8v name = var' i8 name

i1v :: String -> Verif Z3.Node
i1v name = var' i1 name

-- | Named intermediate expression
named :: String -> Z3.Node -> Verif Z3.Node
named str res = do
  v <- var' (Z.getSort res) str
  assign v res
  return v

-- Functions

smin :: Z3.Node -> Z3.Node -> Verif Z3.Node
smin x y = do
  isLess <- Z3.slte x y
  Z3.cond isLess x y

smax :: Z3.Node -> Z3.Node -> Verif Z3.Node
smax x y = do
  isMore <- Z3.sgte x y
  Z3.cond isMore x y

umin :: Z3.Node -> Z3.Node -> Verif Z3.Node
umin x y = do
  isLess <- Z3.ulte x y
  Z3.cond isLess x y

umax :: Z3.Node -> Z3.Node -> Verif Z3.Node
umax x y = do
  isMore <- Z3.ugte x y
  Z3.cond isMore x y

assign :: Z3.Node -> Z3.Node -> Verif ()
assign x y = Z3.eq x y >>= Z3.assert

conjunction :: [Z3.Node] -> Verif Z3.Node
conjunction [] = error "Cannot have a conjunction of zero nodes"
conjunction xs = foldM Z3.and (head xs) (tail xs)

disjunction :: [Z3.Node] -> Verif Z3.Node
disjunction [] = error "Cannot have a disjunction of zero nodes"
disjunction xs = foldM Z3.or (head xs) (tail xs)


