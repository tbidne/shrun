-- | Functional test for a successful run with native logging.
module Functional.Truncation (spec) where

import Functional.Prelude
import Test.Shrun.Verifier
  ( ExpectedText (MkExpectedText),
    ResultText (MkResultText),
  )
import Test.Shrun.Verifier qualified as V

-- | Spec that should run commands successfully and print stdout.
spec :: TestTree
spec =
  testGroup
    "Truncation tests"
    [ testCase "Should truncate command name" $ do
        results <- fmap MkResultText <$> (readIORef =<< run argList)
        V.verifyExpected results expected
    ]
  where
    -- `sleep 1` because commands that run too quickly will not have
    -- output logged
    commands = ["echo \"a long command\" && sleep 1"]
    argList =
      [ "--console-log-cmd-name-trunc",
        "10",
        "--no-config"
      ]
        <> commands

expected :: List ExpectedText
expected =
  MkExpectedText
    <$> [ withSuccessPrefix "echo \"a..."
        ]
