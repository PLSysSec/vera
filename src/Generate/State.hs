{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Generate.State where
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           DSL.DSL
import qualified Z3.Monad                   as Z

data CodegenState = CodegenState { clases  :: Int
                                 , methods :: Int
                                 , fields  :: Int
                                 , ccode   :: Int
                                 , vers    :: M.Map String Int
                                 }

newtype Codegen a = Codegen (StateT CodegenState Verif a)
    deriving (Functor, Applicative, Monad, MonadState CodegenState, MonadIO)

liftVerif :: Verif a -> Codegen a
liftVerif = Codegen . lift

instance Z.MonadZ3 Codegen where
    getSolver = Codegen $ lift $ Z.getSolver
    getContext = Codegen $ lift $ Z.getContext
