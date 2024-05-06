module Functional.Examples.ConsoleLogging (tests) where

import Functional.Prelude
import Test.Shrun.Verifier (ResultText (MkResultText))
import Test.Shrun.Verifier qualified as V

-- NOTE: If tests in this module fail, fix then update configuration.md!

tests :: TestTree
tests =
  testGroup
    "ConsoleLogging"
    [ commandLogOn,
      commandLogOnDefault,
      commandLogOff,
      commandNameTruncN,
      commandLogLineTruncN,
      stripControlAll,
      stripControlNone,
      stripControlSmart
    ]

commandLogOn :: TestTree
commandLogOn =
  testCase "Runs commandLog example with --console-log-command" $ do
    results <- fmap MkResultText <$> (readIORef =<< run args)
    V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "--console-log-command",
          "for i in 1 2; do echo hi; sleep 1; done"
        ]
    expected =
      [ withCommandPrefix "for i in 1 2; do echo hi; sleep 1; done" "hi"
      ]

commandLogOnDefault :: TestTree
commandLogOnDefault =
  testCase "Runs --console-log-command with no output shows default message" $ do
    results <- fmap MkResultText <$> (readIORef =<< run args)
    V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "--console-log-command",
          "for i in 1 2; do sleep 1; done"
        ]
    expected =
      [ withCommandPrefix "for i in 1 2; do sleep 1; done" "Starting..."
      ]

commandLogOff :: TestTree
commandLogOff =
  testCase "Runs commandLog example without --console-log-command" $ do
    results <- fmap MkResultText <$> (readIORef =<< run args)
    V.verifyUnexpected results unexpected
  where
    args =
      withNoConfig
        [ "for i in 1 2; do echo hi; sleep 1; done"
        ]
    unexpected = [commandPrefix]

commandNameTruncN :: TestTree
commandNameTruncN = testCase "Runs --console-log-command-name-trunc 10 example" $ do
  results <- fmap MkResultText <$> (readIORef =<< run args)
  V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "--console-log-command",
          "--console-log-command-name-trunc",
          "10",
          "for i in 1 2 3; do echo hi; sleep 1; done"
        ]
    expected =
      [ withCommandPrefix "for i i..." "hi",
        withSuccessPrefix "for i i..."
      ]

commandLogLineTruncN :: TestTree
commandLogLineTruncN = testCase "Runs --console-log-line-trunc 80 example" $ do
  results <- fmap MkResultText <$> (readIORef =<< run args)
  V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "--console-log-command",
          "--console-log-line-trunc",
          "80",
          "echo 'some ridiculously long command i mean is this really necessary' && sleep 2"
        ]
    expected =
      [ "[Command][echo 'some ridiculously long command i mean is this really necessary' && sleep 2] ..."
      ]

stripControlAll :: TestTree
stripControlAll = testCase "Runs --console-log-strip-control all example" $ do
  results <- fmap MkResultText <$> (readIORef =<< run args)
  V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "--console-log-command",
          "--console-log-command-name-trunc",
          "10",
          "--console-log-strip-control",
          "all",
          "printf ' foo \ESC[35m hello \ESC[3D bye '; sleep 2"
        ]
    -- NOTE: printf over echo -e for portability (echo fails on CI). Also to
    -- try these out manually, note that \ESC will have to be substituted with
    -- \033.
    expected =
      [ withCommandPrefix "printf ..." "foo  hello  bye"
      ]

stripControlNone :: TestTree
stripControlNone = testCase "Runs --console-log-strip-control none example" $ do
  results <- fmap MkResultText <$> (readIORef =<< run args)
  V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "--console-log-command",
          "--console-log-command-name-trunc",
          "10",
          "--console-log-strip-control",
          "none",
          "printf ' foo \ESC[35m hello \ESC[3D bye '; sleep 2"
        ]
    expected =
      [ withCommandPrefix "printf ..." "foo \ESC[35m hello \ESC[3D bye"
      ]

stripControlSmart :: TestTree
stripControlSmart = testCase "Runs --console-log-strip-control smart example" $ do
  results <- fmap MkResultText <$> (readIORef =<< run args)
  V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "--console-log-command",
          "--console-log-command-name-trunc",
          "10",
          "--console-log-strip-control=smart",
          "printf ' foo \ESC[35m hello \ESC[3D bye '; sleep 2"
        ]
    expected =
      [ withCommandPrefix "printf ..." "foo \ESC[35m hello  bye"
      ]
