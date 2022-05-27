-- | Functional test for a run that should timeout.
module Functional.Timeout (spec) where

import Functional.Prelude
import Functional.Utils qualified as U
import Test.ShellRun.Verifier (ExpectedText (..), ResultText (..))
import Test.ShellRun.Verifier qualified as V
import Test.Tasty.HUnit qualified as THU

-- | Spec that should timeout.
spec :: TestTree
spec = THU.testCase "Should time out" $ do
  results <- fmap MkResultText <$> (readIORef =<< U.runAndGetLogs argList)

  V.verifyExpected results allExpected
  where
    argList = timeout : commands
    commands = ["sleep 10"]
    timeout = "--timeout=5"

allExpected :: List ExpectedText
allExpected =
  MkExpectedText
    <$> [ U.cancelled,
          U.totalTime
        ]
