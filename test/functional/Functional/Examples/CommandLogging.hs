module Functional.Examples.CommandLogging (tests) where

import Data.Text qualified as T
import Functional.Prelude
import Test.Shrun.Verifier qualified as V

-- NOTE: If tests in this module fail, fix then update configuration.md!

tests :: TestTree
tests =
  testGroup
    "CommandLogging"
    [ readSizeDefault,
      readSize
    ]

readSizeDefault :: TestTree
readSizeDefault =
  testCase "Default --read-size splits 16,000" $ do
    results <- run args
    V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "--console-log-command",
          "--console-log-command-name-trunc",
          "5",
          cmd
        ]
    commandLog = replicate 16_000 'a'
    cmd = "sleep 1 ; echo " ++ commandLog ++ "b; sleep 1"
    cmdExpected = V.MkExpectedText . T.pack $ commandLog
    expected =
      [ withCommandPrefix "sl..." cmdExpected,
        withCommandPrefix "sl..." "b"
      ]

readSize :: TestTree
readSize =
  testCase "Runs --command-log-read-size example" $ do
    results <- run args
    V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "--console-log-command",
          "--command-log-read-size",
          "5b",
          "--command-log-poll-interval",
          "1_000_000",
          cmd
        ]
    cmd :: (IsString a) => a
    cmd = "echo abcdef && sleep 3"
    expected =
      [ withCommandPrefix cmd "abcde",
        withCommandPrefix cmd "f"
      ]
