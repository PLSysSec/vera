module Utils where
import           BenchUtils
import           Control.Monad (forM_, unless)
import qualified Data.Map      as M
import qualified DSL.DSL       as D

vtest :: String -> D.SMTResult -> M.Map String Integer -> IO ()
vtest name result expectedVars = case result of
  D.SolverUnsat -> error "Expected SAT but got UNSAT"
  D.SolverFailed -> error "Expected SAT but the solver failed"
  D.SolverSat actualVars -> do
    forM_ (M.toList expectedVars) $ \(expectedVar, expectedVal) ->
      case M.lookup expectedVar actualVars of
        Nothing -> error $ unwords ["Expected to find", show expectedVar]
        Just actualVal -> unless (actualVal == expectedVal) $
                          error $ unwords ["Expected"
                                          , show expectedVar
                                          , "to be"
                                          , show expectedVal
                                          , "but got"
                                          , show actualVal
                                          ]
