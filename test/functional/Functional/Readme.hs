{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Functional tests for readme examples.
module Functional.Readme (specs) where

import DBus.Notify (UrgencyLevel (..))
import Data.Text qualified as T
import Functional.Prelude
import Functional.TestArgs (TestArgs (..))
import Shrun.Notify.MonadNotify (ShrunNote (..))
import Shrun.Notify.Types (NotifyTimeout (..))
import Test.Shrun.Verifier (ResultText (..))
import Test.Shrun.Verifier qualified as V

-- NOTE: If tests in this module fail, fix then update the README!

-- | Specs from readme
specs :: IO TestArgs -> TestTree
specs args =
  testGroup
    "README examples"
    ( [ gif,
        core,
        timeout,
        initOn,
        initOff,
        cmdlogOn,
        cmdlogOff,
        fileLog args,
        keyHideOn,
        keyHideOff,
        stripControlAlwaysCmdNames,
        stripControlAll,
        stripControlNone,
        stripControlSmart,
        fileLogStripControlAll args,
        fileLogStripControlNone args,
        fileLogStripControlSmart args,
        cmdNameTruncN,
        cmdLogLineTruncN
      ]
        <> notifyTests
    )

gif :: TestTree
gif =
  testCase "Runs gif example" $ do
    results <- fmap MkResultText <$> (readIORef =<< runExitFailure args)
    V.verifyExpected results expected
  where
    args =
      withBaseArgs
        [ "sign-peace-treaty",
          "takeover"
        ]
    expected =
      [ withFinishedPrefix "13 seconds",
        withSuccessPrefix "skynet",
        withSuccessPrefix "ui",
        -- NOTE: The final error message is unreliable, so we would occasionally
        -- run into an error where we received a different message. This led
        -- to the mistaken belief that there was a concurrency bug, which is
        -- perhaps technically true, but it is regarding messages we receive,
        -- not from any output getting mangled.
        withErrorPrefix "sign-peace-treaty",
        withCommandPrefix "skynet" "preparing nuclear missil-- i mean gift baskets",
        withCommandPrefix "ui" "adding emojis. we like to have fun :-)",
        withCommandPrefix "querying-targets" "finding targets...",
        withCommandPrefix "sign-peace-treaty" "play it cool...",
        withTimerPrefix "8 seconds"
      ]

core :: TestTree
core =
  testCase "Runs core example" $ do
    results <- fmap MkResultText <$> (readIORef =<< runExitFailure args)
    V.verifyExpected results expected
  where
    args =
      withBaseArgs
        [ "all",
          "echo cat"
        ]
    expected =
      [ withSuccessPrefix "echo cat",
        withSuccessPrefix "echo hi",
        withSuccessPrefix "cmd1",
        withErrorPrefix "cmd4",
        withFinishedPrefix "0 seconds"
      ]

timeout :: TestTree
timeout =
  testCase "Runs timeout example" $ do
    results <- fmap MkResultText <$> (readIORef =<< runExitFailure args)
    V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "-t",
          "8",
          "sleep 5",
          "sleep 10",
          "sleep 15"
        ]
    expected =
      [ withSuccessPrefix "sleep 5",
        withTimeoutPrefix "sleep 10, sleep 15",
        finishedPrefix
      ]

initOn :: TestTree
initOn =
  testCase "Runs init successful example" $ do
    results <- fmap MkResultText <$> (readIORef =<< run args)
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
    results <- fmap MkResultText <$> (readIORef =<< runExitFailure args)
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

cmdlogOn :: TestTree
cmdlogOn =
  testCase "Runs cmdlog example with --cmd-log" $ do
    results <- fmap MkResultText <$> (readIORef =<< run args)
    V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "--cmd-log",
          "for i in {1..10}; do echo hi; sleep 1; done"
        ]
    expected =
      [ withCommandPrefix "for i in {1..10}; do echo hi; sleep 1; done" "hi"
      ]

cmdlogOff :: TestTree
cmdlogOff =
  testCase "Runs cmdlog example without --cmd-log" $ do
    results <- fmap MkResultText <$> (readIORef =<< run args)
    V.verifyUnexpected results unexpected
  where
    args =
      withNoConfig
        [ "for i in {1..10}; do echo hi; sleep 1; done"
        ]
    unexpected = [commandPrefix]

fileLog :: IO TestArgs -> TestTree
fileLog testArgs = testCase "Runs file-log example" $ do
  outFile <- (</> "readme-file-out.log") . view #tmpDir <$> testArgs
  let args =
        withNoConfig
          [ "--file-log",
            outFile,
            "sleep 2",
            "bad",
            "for i in {1..3}; do echo hi; sleep 1; done"
          ]

  resultsConsole <- fmap MkResultText <$> (readIORef =<< runExitFailure args)
  V.verifyExpected resultsConsole expectedConsole

  resultsFile <- fmap MkResultText . T.lines <$> readFileUtf8ThrowM outFile
  V.verifyExpected resultsFile expectedFile
  where
    expectedConsole =
      [ withErrorPrefix "bad",
        withSuccessPrefix "sleep 2",
        withSuccessPrefix "for i in {1..3}; do echo hi; sleep 1; done",
        finishedPrefix
      ]
    expectedFile =
      expectedConsole
        ++ [ withCommandPrefix "for i in {1..3}; do echo hi; sleep 1; done" "hi"
           ]

keyHideOn :: TestTree
keyHideOn =
  testCase "Runs key hide example with --key-hide" $ do
    results <- fmap MkResultText <$> (readIORef =<< run args)
    V.verifyExpectedUnexpected results expected unexpected
  where
    args =
      withBaseArgs
        [ "--key-hide",
          "skynet"
        ]
    expected =
      [ withCommandPrefix
          "echo \"preparing nuclear missil-- i mean gift baskets\"; sleep 13"
          "preparing nuclear missil-- i mean gift baskets",
        withSuccessPrefix "echo \"preparing nuclear missil-- i mean gift baskets\"; sleep 13"
      ]
    unexpected =
      [ withCommandPrefix "skynet" "",
        withSuccessPrefix "skynet"
      ]

keyHideOff :: TestTree
keyHideOff =
  testCase "Runs key hide example without --key-hide" $ do
    results <- fmap MkResultText <$> (readIORef =<< run args)
    V.verifyExpectedUnexpected results expected unexpected
  where
    args =
      withBaseArgs
        [ "skynet"
        ]
    expected =
      [ withCommandPrefix "skynet" "",
        withSuccessPrefix "skynet"
      ]
    unexpected =
      [ withCommandPrefix
          "echo \"preparing nuclear missil-- i mean gift baskets\"; sleep 13"
          "preparing nuclear missil-- i mean gift baskets",
        withSuccessPrefix "echo \"preparing nuclear missil-- i mean gift baskets\"; sleep 13"
      ]

stripControlAll :: TestTree
stripControlAll = testCase "Runs --cmd-log-strip-control all example" $ do
  results <- fmap MkResultText <$> (readIORef =<< run args)
  V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "-lx10",
          "--cmd-log-strip-control",
          "all",
          "printf ' foo \ESC[35m hello \ESC[3D bye '; sleep 5"
        ]
    -- NOTE: printf over echo -e for portability (echo fails on CI). Also to
    -- try these out manually, note that \ESC will have to be substituted with
    -- \033.
    expected =
      [ withCommandPrefix "printf ..." "foo  hello  bye"
      ]

-- NOTE: Not strictly a README test, but this is valuable to have and as it
-- is a slight variation on the ones here, we leave it in.
stripControlAlwaysCmdNames :: TestTree
stripControlAlwaysCmdNames = testCase "Always strips command names" $ do
  results <- fmap MkResultText <$> (readIORef =<< run args)
  V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "-l",
          "--cmd-log-strip-control",
          "none",
          "printf ' foo \ESC[35m hello \ESC[3D bye '; sleep 5"
        ]
    -- i.e. ansi codes are not being stripped (because =none), yet they are
    -- gone from the command names
    expected =
      [ withCommandPrefix "printf ' foo  hello  bye '; sleep 5" "foo \ESC[35m hello \ESC[3D bye",
        withSuccessPrefix "printf ' foo  hello  bye '; sleep 5"
      ]

stripControlNone :: TestTree
stripControlNone = testCase "Runs --cmd-log-strip-control none example" $ do
  results <- fmap MkResultText <$> (readIORef =<< run args)
  V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "-lx10",
          "--cmd-log-strip-control",
          "none",
          "printf ' foo \ESC[35m hello \ESC[3D bye '; sleep 5"
        ]
    expected =
      [ withCommandPrefix "printf ..." "foo \ESC[35m hello \ESC[3D bye"
      ]

stripControlSmart :: TestTree
stripControlSmart = testCase "Runs --cmd-log-strip-control smart example" $ do
  results <- fmap MkResultText <$> (readIORef =<< run args)
  V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "-lx10",
          "--cmd-log-strip-control=smart",
          "printf ' foo \ESC[35m hello \ESC[3D bye '; sleep 5"
        ]
    expected =
      [ withCommandPrefix "printf ..." "foo \ESC[35m hello  bye"
      ]

fileLogStripControlAll :: IO TestArgs -> TestTree
fileLogStripControlAll testArgs = testCase "Runs file-log strip-control all example" $ do
  outFile <- (</> "readme-file-out-strip-control-all.log") . view #tmpDir <$> testArgs
  let args =
        withNoConfig
          [ "--file-log",
            outFile,
            "--file-log-strip-control",
            "all",
            "printf ' foo \ESC[35m hello \ESC[3D bye '; sleep 5"
          ]

  _ <- fmap MkResultText <$> (readIORef =<< run args)

  resultsFile <- fmap MkResultText . T.lines <$> readFileUtf8ThrowM outFile
  V.verifyExpected resultsFile expectedFile
  where
    expectedFile =
      [ withCommandPrefix "printf ' foo  hello  bye '; sleep 5" "foo  hello  bye"
      ]

fileLogStripControlNone :: IO TestArgs -> TestTree
fileLogStripControlNone testArgs = testCase "Runs file-log strip-control none example" $ do
  outFile <- (</> "readme-file-out-strip-control-none.log") . view #tmpDir <$> testArgs
  let args =
        withNoConfig
          [ "--file-log",
            outFile,
            "--file-log-strip-control",
            "none",
            "printf ' foo \ESC[35m hello \ESC[3D bye '; sleep 5"
          ]

  _ <- fmap MkResultText <$> (readIORef =<< run args)

  resultsFile <- fmap MkResultText . T.lines <$> readFileUtf8ThrowM outFile
  V.verifyExpected resultsFile expectedFile
  where
    expectedFile =
      [ withCommandPrefix
          "printf ' foo  hello  bye '; sleep 5"
          "foo \ESC[35m hello \ESC[3D bye"
      ]

fileLogStripControlSmart :: IO TestArgs -> TestTree
fileLogStripControlSmart testArgs = testCase "Runs file-log strip-control smart example" $ do
  outFile <- (</> "readme-file-out-strip-control-smart.log") . view #tmpDir <$> testArgs
  let args =
        withNoConfig
          [ "--file-log",
            outFile,
            "--file-log-strip-control",
            "smart",
            "printf ' foo \ESC[35m hello \ESC[3D bye '; sleep 5"
          ]

  _ <- fmap MkResultText <$> (readIORef =<< run args)

  resultsFile <- fmap MkResultText . T.lines <$> readFileUtf8ThrowM outFile
  V.verifyExpected resultsFile expectedFile
  where
    expectedFile =
      [ withCommandPrefix
          "printf ' foo  hello  bye '; sleep 5"
          "foo \ESC[35m hello  bye"
      ]

cmdNameTruncN :: TestTree
cmdNameTruncN = testCase "Runs --cmd-name-trunc 10 example" $ do
  results <- fmap MkResultText <$> (readIORef =<< run args)
  V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "-l",
          "--cmd-name-trunc",
          "10",
          "for i in {1..3}; do echo hi; sleep 1; done"
        ]
    expected =
      [ withCommandPrefix "for i i..." "hi",
        withSuccessPrefix "for i i..."
      ]

cmdLogLineTruncN :: TestTree
cmdLogLineTruncN = testCase "Runs --cmd-log-line-trunc 80 example" $ do
  results <- fmap MkResultText <$> (readIORef =<< run args)
  V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "-l",
          "--cmd-log-line-trunc",
          "80",
          "echo 'some ridiculously long command i mean is this really necessary' && sleep 5"
        ]
    expected =
      [ "[Command][echo 'some ridiculously long command i mean is this really necessar..."
      ]

notifyTests :: [TestTree]
#if OSX
notifyTests = []
#else
notifyTests =
  [ notifyActionFinal,
    notifyTimeoutNever
  ]

-- NOTE: There is no DBus test because that requires creating a real DBus
-- connection, as we are running in IO.

notifyActionFinal :: TestTree
notifyActionFinal = testCase "Runs --notify-action final" $ do
  results <- readIORef =<< runNotes args
  expected @=? results
  where
    args =
      withNoConfig
        [ "--notify-system",
          "notify-send",
          "--notify-action",
          "final",
          "sleep 2",
          "sleep 3"
        ]
    expected =
      [ MkShrunNote
          { summary = "Shrun Finished",
            body = "3 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 10
          }
      ]

notifyTimeoutNever :: TestTree
notifyTimeoutNever = testCase "Runs --notify-timeout never" $ do
  results <- readIORef =<< runNotes args
  expected @=? results
  where
    args =
      withNoConfig
        [ "--notify-system",
          "notify-send",
          "--notify-action",
          "command",
          "--notify-timeout",
          "never",
          "sleep 2",
          "sleep 3"
        ]
    expected =
      [ MkShrunNote
          { summary = "Shrun Finished",
            body = "3 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutNever
          },
        MkShrunNote
          { summary = "[sleep 3]  Finished",
            body = "3 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutNever
          },
        MkShrunNote
          { summary = "[sleep 2]  Finished",
            body = "2 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutNever
          }
      ]
#endif
