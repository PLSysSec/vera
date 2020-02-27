module Sanity (sanityCheck) where
import           ActiveCode.JavaScript
import           BenchUtils
import           Buggy
import           Cpp
import           GenIonMonkey
import           JavaScript
import           Test.Tasty.HUnit
import           Utils

sanityCheck = benchTestGroup "Sanity" [ signTests
                                      , brokenCeilTest
                                      , jsBinOpTest JSAdd
                                      ]
