-- | Functional test for a successful run with native logging.
module Functional.Truncation (spec) where

import Functional.Prelude
import Functional.Utils qualified as U
import Test.ShellRun.Verifier (ExpectedText (..), ResultText (..))
import Test.ShellRun.Verifier qualified as V
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as THU

-- | Spec that should run commands successfully and print stdout.
spec :: TestTree
spec =
  Tasty.testGroup
    "Truncation tests"
    [ THU.testCase "Should truncate command name" $ do
        results <- fmap MkResultText <$> (readIORef =<< U.runAndGetLogs argList)
        V.verifyExpected results expected
    ]
  where
    -- `sleep 1` because commands that run too quickly will not have
    -- output logged
    commands = ["echo \"a long command\" && sleep 1"]
    argList =
      [ "-x 10"
      ]
        <> commands

expected :: List ExpectedText
expected =
  MkExpectedText
    <$> [ U.infoSuccessPrefix "echo \"a..."
        ]
