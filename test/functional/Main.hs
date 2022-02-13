-- | Runs functional tests.
module Main (main) where

import Constants qualified
import ShellRun.Prelude
import Success qualified
import SuccessCommandLogging qualified
import SuccessShowKey qualified
import System.IO qualified as IO
import System.IO.Silently qualified as Shh
import System.Process qualified as P
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Timeout qualified

-- | Entry point for functional tests.
main :: IO ()
main =
  Tasty.defaultMain $ Tasty.withResource setup tearDown $ const specs

specs :: TestTree
specs =
  Tasty.testGroup
    "Functional Tests"
    [ Success.spec,
      SuccessShowKey.spec,
      SuccessCommandLogging.spec,
      Timeout.spec
    ]

setup :: IO ()
setup =
  let proc = (P.shell "./setup_legend.sh") {P.cwd = Just Constants.workingDirectory}
   in Shh.hSilence [IO.stderr] (P.readCreateProcess proc "" $> ())

tearDown :: () -> IO ()
tearDown _ =
  let proc = (P.shell "./teardown_legend.sh") {P.cwd = Just Constants.workingDirectory}
   in P.readCreateProcess proc "" $> ()
