module Utils where
import           BenchUtils
import           Control.Monad                       (forM_, unless)
import qualified Data.Map                            as M
import qualified DSL.DSL                             as D
import           Generate.State
import           IonMonkeyGenerated.VerifyIndividual

mkFloatTests :: String -> TestFunction -> BenchTest
mkFloatTests testGroupName testFn =
  benchTestGroup testGroupName
    [makeTest (testGroupName ++ " lower i32") $ testLower testFn
    -- , makeTest (testGroupName ++ " float lower i32") $ testFlLower testFn
    -- , makeTest (testGroupName ++ " float upper i32") $ testFlUpper testFn
    , makeTest (testGroupName ++ " upper i32") $ testUpper testFn
    , makeTest (testGroupName ++ " UB") $ testUB testFn
    , makeTest (testGroupName ++ " low invariant") $ testLowInvariant testFn
    , makeTest (testGroupName ++ " high invariant") $ testHighInvariant testFn
    , makeTest (testGroupName ++ " negative zero") $ testNegZ testFn
    , makeTest (testGroupName ++ " nan") $ testNan testFn
    , makeTest (testGroupName ++ " inf") $ testInf testFn
    , makeTest (testGroupName ++ " fract") $ testFract testFn
    , makeTest (testGroupName ++ " exp") $ testExp testFn
    ]
  where makeTest str act = benchTestCase str $ evalCodegen Nothing act

mki32Tests :: String -> TestFunction -> BenchTest
mki32Tests testGroupName testFn =
  benchTestGroup testGroupName
    [ makeTest (testGroupName ++ " lower i32") $ testLower testFn
    , makeTest (testGroupName ++ " upper i32") $ testUpper testFn
    , makeTest (testGroupName ++ " UB") $ testUB testFn
    ]
  where makeTest str act = benchTestCase str $ evalCodegen Nothing act

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
    forM_ (M.toList expectedVars) $ \(expectedVar, expectedVal) -> do
      case M.lookup expectedVar actualVars of
        Nothing -> error $ unwords ["Expected to find"
                                   , show expectedVar
                                   , "in"
                                   , show actualVars
                                   ]
        Just actualVal -> do
          case expectedVal of
            -0.0 -> case actualVal of
                    -0.0 -> return ()
                    _  -> error $ unwords [ "Expected"
                                          , show expectedVar
                                          , "to be negative zero but got"
                                          , show actualVal
                                          ]
            _ -> return ()

          case actualVal of
            -0.0 -> case expectedVal of
                    -0.0 -> return ()
                    _  -> error $ unwords [ "Expected"
                                          , show expectedVar
                                          , "to be"
                                          , show expectedVal
                                          , "but got negative zero"
                                          ]
            _ -> return ()

          unless (actualVal == expectedVal ||
                  (isNaN actualVal && isNaN expectedVal)) $
            error $ unwords ["Expected"
                            , show expectedVar
                            , "to be"
                            , show expectedVal
                            , "but got"
                            , show actualVal
                            ]
