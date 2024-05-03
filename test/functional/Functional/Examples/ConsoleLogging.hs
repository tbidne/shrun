module Functional.Examples.ConsoleLogging (tests) where

import Functional.Prelude
import Test.Shrun.Verifier (ResultText (MkResultText))
import Test.Shrun.Verifier qualified as V

-- NOTE: If tests in this module fail, fix then update configuration.md!

tests :: TestTree
tests =
  testGroup
    "ConsoleLogging"
    [ cmdlogOn,
      cmdlogOnDefault,
      cmdlogOff,
      cmdNameTruncN,
      cmdLogLineTruncN,
      stripControlAll,
      stripControlNone,
      stripControlSmart
    ]

cmdlogOn :: TestTree
cmdlogOn =
  testCase "Runs cmdlog example with --console-log-cmd" $ do
    results <- fmap MkResultText <$> (readIORef =<< run args)
    V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "--console-log-cmd",
          "for i in 1 2; do echo hi; sleep 1; done"
        ]
    expected =
      [ withCommandPrefix "for i in 1 2; do echo hi; sleep 1; done" "hi"
      ]

cmdlogOnDefault :: TestTree
cmdlogOnDefault =
  testCase "Runs --console-log-cmd with no output shows default message" $ do
    results <- fmap MkResultText <$> (readIORef =<< run args)
    V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "--console-log-cmd",
          "for i in 1 2; do sleep 1; done"
        ]
    expected =
      [ withCommandPrefix "for i in 1 2; do sleep 1; done" "Starting..."
      ]

cmdlogOff :: TestTree
cmdlogOff =
  testCase "Runs cmdlog example without --console-log-cmd" $ do
    results <- fmap MkResultText <$> (readIORef =<< run args)
    V.verifyUnexpected results unexpected
  where
    args =
      withNoConfig
        [ "for i in 1 2; do echo hi; sleep 1; done"
        ]
    unexpected = [commandPrefix]

cmdNameTruncN :: TestTree
cmdNameTruncN = testCase "Runs --console-log-cmd-name-trunc 10 example" $ do
  results <- fmap MkResultText <$> (readIORef =<< run args)
  V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "--console-log-cmd",
          "--console-log-cmd-name-trunc",
          "10",
          "for i in 1 2 3; do echo hi; sleep 1; done"
        ]
    expected =
      [ withCommandPrefix "for i i..." "hi",
        withSuccessPrefix "for i i..."
      ]

cmdLogLineTruncN :: TestTree
cmdLogLineTruncN = testCase "Runs --console-log-line-trunc 80 example" $ do
  results <- fmap MkResultText <$> (readIORef =<< run args)
  V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "--console-log-cmd",
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
        [ "--console-log-cmd",
          "--console-log-cmd-name-trunc",
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
        [ "--console-log-cmd",
          "--console-log-cmd-name-trunc",
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
        [ "--console-log-cmd",
          "--console-log-cmd-name-trunc",
          "10",
          "--console-log-strip-control=smart",
          "printf ' foo \ESC[35m hello \ESC[3D bye '; sleep 2"
        ]
    expected =
      [ withCommandPrefix "printf ..." "foo \ESC[35m hello  bye"
      ]
