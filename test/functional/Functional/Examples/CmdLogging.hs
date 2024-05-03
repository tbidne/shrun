module Functional.Examples.CmdLogging (tests) where

import Data.Text qualified as T
import Functional.Prelude
import Test.Shrun.Verifier (ResultText (MkResultText))
import Test.Shrun.Verifier qualified as V

-- NOTE: If tests in this module fail, fix then update configuration.md!

tests :: TestTree
tests =
  testGroup
    "CmdLogging"
    [ cmdLogReadSizeDefault,
      cmdLogReadSize
    ]

cmdLogReadSizeDefault :: TestTree
cmdLogReadSizeDefault =
  testCase "Default --read-size splits 1024" $ do
    results <- fmap MkResultText <$> (readIORef =<< run args)
    V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "--console-log-cmd",
          "--console-log-cmd-name-trunc",
          "5",
          cmd
        ]
    commandLog = replicate 1024 'a'
    cmd = "sleep 1 ; echo " ++ commandLog ++ "b; sleep 1"
    cmdExpected = V.MkExpectedText . T.pack $ commandLog
    expected =
      [ withCommandPrefix "sl..." cmdExpected,
        withCommandPrefix "sl..." "b"
      ]

cmdLogReadSize :: TestTree
cmdLogReadSize =
  testCase "Runs --cmd-log-read-size example" $ do
    results <- fmap MkResultText <$> (readIORef =<< run args)
    V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "--console-log-cmd",
          "--cmd-log-read-size",
          "5",
          "--cmd-log-poll-interval",
          "1000000",
          cmd
        ]
    cmd :: (IsString a) => a
    cmd = "echo abcdef && sleep 3"
    expected =
      [ withCommandPrefix cmd "abcde",
        withCommandPrefix cmd "f"
      ]
