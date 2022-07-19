-- | Functional test for a successful run.
module Functional.SuccessNoLogging (spec) where

import Functional.Prelude
import Functional.Utils qualified as U

-- | Spec that should run commands successfully.
spec :: TestTree
spec =
  testCase "Should run commands successfully without logging" $ do
    let argList = ["--cmd-log", "--disable-log", "--no-config"] <> commands

    results <- readIORef =<< U.runAndGetLogs argList

    [] @=? results
  where
    commands = ["sleep 1 && echo hi && sleep 2"]
