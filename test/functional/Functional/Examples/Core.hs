-- | Functional tests for readme examples.
module Functional.Examples.Core (tests) where

import Functional.Prelude
import Test.Shrun.Verifier qualified as V

-- NOTE: If tests in this module fail, fix then update configuration.md!

tests :: TestTree
tests =
  testGroup
    "CoreConfig"
    [ initOn,
      initOff,
      timeout
    ]

initOn :: TestTree
initOn =
  testCase "Runs init successful example" $ do
    results <- run args
    V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "--init",
          ". examples/bashrc",
          "bash_function"
        ]
    expected =
      [ withSuccessPrefix "bash_function",
        finishedPrefix
      ]

initOff :: TestTree
initOff =
  testCase "Runs init failure example" $ do
    results <- runExitFailure args
    V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "bash_function"
        ]
    expected =
      [ withErrorPrefix "bash_function",
        finishedPrefix
      ]

timeout :: TestTree
timeout =
  testCase "Runs timeout example" $ do
    results <- runExitFailure args
    V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "-t",
          "4",
          "sleep 2",
          "sleep 6",
          "sleep 8"
        ]
    expected =
      [ withSuccessPrefix "sleep 2",
        withTimeoutPrefix "sleep 6, sleep 8",
        finishedPrefix
      ]
