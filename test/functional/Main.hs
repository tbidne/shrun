-- | Runs functional tests.
module Main (main) where

import ShellRun.Prelude
import Success qualified
import SuccessCommandLogging qualified
import SuccessShowKey qualified
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Timeout qualified

-- | Entry point for functional tests.
main :: IO ()
main =
  Tasty.defaultMain specs

specs :: TestTree
specs =
  Tasty.testGroup
    "Functional Tests"
    [ Success.spec,
      SuccessShowKey.spec,
      SuccessCommandLogging.spec,
      Timeout.spec
    ]
