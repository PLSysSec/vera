module Utils where
import           BenchUtils
import           Control.Monad (forM_, unless)
import qualified Data.Map      as M
import qualified DSL.DSL       as D

satTest :: D.SMTResult -> IO ()
satTest D.SolverSat{} = return ()
satTest e = error $ unwords ["Expected a SAT result but got", show e]

unsatTest :: D.SMTResult -> IO ()
unsatTest D.SolverUnsat{} = return ()
unsatTest e = error $ unwords ["Expected a UNSAT result but got", show e]

vtest :: D.SMTResult -> M.Map String Double -> IO ()
vtest result expectedVars = case result of
  D.SolverUnsat -> error "Expected SAT but got UNSAT"
  D.SolverFailed -> error "Expected SAT but the solver failed"
  D.SolverSat actualVars -> do
    forM_ (M.toList expectedVars) $ \(expectedVar, expectedVal) ->
      case M.lookup expectedVar actualVars of
        Nothing -> error $ unwords ["Expected to find"
                                   , show expectedVar
                                   , "in"
                                   , show actualVars
                                   ]
        Just actualVal -> unless (actualVal == expectedVal ||
                                 (isNaN actualVal && isNaN expectedVal)) $
                          error $ unwords ["Expected"
                                          , show expectedVar
                                          , "to be"
                                          , show expectedVal
                                          , "but got"
                                          , show actualVal
                                          ]
