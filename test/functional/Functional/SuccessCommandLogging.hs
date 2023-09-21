-- | Functional test for a successful run with native logging.
module Functional.SuccessCommandLogging (spec) where

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
    "Command logging tests"
    [ success,
      capturesStderr
    ]

success :: TestTree
success = testCase "Should print commands stdout" $ do
  let argList =
        [ "--no-config",
          "sleep 2",
          cmdLogging
        ]
          <> commands

  results <- fmap MkResultText <$> (readIORef =<< run argList)

  V.verifyExpected results expected
  where
    -- `sleep 1` is because commands that run too quickly will not have
    -- output logged
    commands = ["echo hi && sleep 1"]
    cmdLogging = "--cmd-log"

    expected =
      MkExpectedText
        <$> [ withCommandPrefix "echo hi && sleep 1" "hi"
            ]

capturesStderr :: TestTree
capturesStderr = testCase "Should capture stdout and stderr" $ do
  let argList =
        [ "--no-config",
          "--cmd-log",
          "./test/functional/Functional/stderr.sh"
        ]

  results <- fmap MkResultText <$> (readIORef =<< run argList)

  V.verifyExpected results expected
  where
    expected =
      [ withCommandPrefix "./test/functional/Functional/stderr.sh" "stdout 1",
        withCommandPrefix "./test/functional/Functional/stderr.sh" "stderr 1",
        withCommandPrefix "./test/functional/Functional/stderr.sh" "stdout 2",
        withCommandPrefix "./test/functional/Functional/stderr.sh" "stderr 2"
      ]
