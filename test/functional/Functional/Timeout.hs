-- | Functional test for a run that should timeout.
module Functional.Timeout (spec) where

import Functional.Prelude
import Test.Shrun.Verifier (ExpectedText (..), ResultText (..))
import Test.Shrun.Verifier qualified as V

-- | Spec that should timeout.
spec :: TestTree
spec = testCase "Should time out" $ do
  results <- fmap MkResultText <$> (readIORef =<< runAndGetLogsExitFailure argList)

  V.verifyExpected results allExpected
  where
    argList = "--no-config" : timeout : commands
    commands = ["sleep 10"]
    timeout = "--timeout=5"

allExpected :: List ExpectedText
allExpected =
  MkExpectedText
    <$> [ withTimeoutPrefix "sleep 10",
          finishedPrefix
        ]
