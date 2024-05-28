-- | Misc tests
module Functional.Miscellaneous (specs) where

import Functional.Prelude
import Test.Shrun.Verifier qualified as V

specs :: TestTree
specs =
  testGroup
    "Miscellaneous"
    [ splitNewlineLogs,
      spaceErrorLogs,
      stripControlAlwaysCmdNames,
      reportsStderr,
      slowOutputBroken
    ]

splitNewlineLogs :: TestTree
splitNewlineLogs = testCase "Logs with newlines are split" $ do
  results <- run args
  V.verifyExpectedUnexpected results expected unexpected
  where
    args =
      withNoConfig
        [ "--console-log-command",
          "sleep 1 && echo 'line one\nline two' && sleep 2"
        ]

    -- Newline is stripped from printed result
    printedCmd :: (IsString a) => a
    printedCmd = "sleep 1 && echo 'line one line two' && sleep 2"

    expected =
      [ withSuccessPrefix printedCmd,
        withCommandPrefix printedCmd "line one",
        withCommandPrefix printedCmd "line two"
      ]
    unexpected =
      [ withCommandPrefix printedCmd "line one line two"
      ]

spaceErrorLogs :: TestTree
spaceErrorLogs = testCase "Error Log with newlines is spaced" $ do
  results <- runExitFailure args
  V.verifyExpectedUnexpected results expected unexpected
  where
    args =
      withNoConfig
        [ "--console-log-command",
          "sleep 1 && echo 'abc\n  def' && sleep 1 && exit 1"
        ]

    -- Verifying final 'abc\n  def' log is translated to 'abc  def' in the final
    -- error msg. Breakdown:
    --
    -- - In command names, newlines are converted to spaces, so 'abc\n  def'
    --   'abc   def'.
    --
    -- - In command logs, newlines are split across separate logs. Hence
    --   'abc' and '  def'.
    --
    -- - In the final error message, it appears newlines are just stripped?
    --   Should probably be the same as command names.
    expected =
      [ withErrorPrefix "sleep 1 && echo 'abc   def' && sleep 1 && exit 1" <> "2 seconds: abc   def",
        withCommandPrefix "sleep 1 && echo 'abc   def' && sleep 1 && exit 1" "abc",
        withCommandPrefix "sleep 1 && echo 'abc   def' && sleep 1 && exit 1" "  def"
      ]
    unexpected =
      [ withErrorPrefix "sleep 1 && echo 'abc def' && sleep 1 && exit 1" <> "2 seconds: abcdef"
      ]

-- NOTE: This used to be in Examples (subsequently Examples.ConsoleLogging),
-- as it fit in alongside the other tests. However, we prefer those tests to
-- match Configuration.md as closely as possible, to make maintaining the
-- markdown file as easy as possible. Thus we move it here.
stripControlAlwaysCmdNames :: TestTree
stripControlAlwaysCmdNames = testCase "Always strips command names" $ do
  results <- run args
  V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "--console-log-command",
          "--console-log-strip-control",
          "none",
          "printf ' foo \ESC[35m hello \ESC[3D bye '; sleep 2"
        ]
    -- i.e. ansi codes are not being stripped (because =none), yet they are
    -- gone from the command names
    expected =
      [ withCommandPrefix "printf ' foo  hello  bye '; sleep 2" "foo \ESC[35m hello \ESC[3D bye",
        withSuccessPrefix "printf ' foo  hello  bye '; sleep 2"
      ]

-- Tests that we default to stderr when it exists.
-- See NOTE: [Stderr reporting].
reportsStderr :: TestTree
reportsStderr = testCase "Reports stderr" $ do
  results <- runExitFailure args
  V.verifyExpectedUnexpected results expected unexpected
  where
    scriptPath :: (IsString a) => a
    scriptPath = "./test/functional/Functional/stderr.sh"

    args =
      withNoConfig
        [ "--console-log-command",
          scriptPath
        ]

    expected =
      [ withErrorPrefix scriptPath <> "1 second: some stderr",
        withCommandPrefix scriptPath "some output",
        withCommandPrefix scriptPath "some stderr",
        withCommandPrefix scriptPath "more output"
      ]

    -- Really the first one is what we should get if we are ignoring stderr,
    -- but we include all for paranoia (we only want the above 'some stderr').
    unexpected =
      [ withErrorPrefix scriptPath <> "1 second: some output more output",
        withErrorPrefix scriptPath <> "1 second: some output",
        withErrorPrefix scriptPath <> "1 second: more output"
      ]

-- This is an "anti" test. We actually do not want this behavior. Ideally,
-- the lines:
--
--   echo -n "first: "
--   sleep 1
--   echo "second"
--
-- Should produce "first: second", since that is the original intention
-- (-n means do not append a newline). But because the time between logs is
-- longer than the poll-interval, it is split.
--
-- We include this test for documenting the current behavior, and if we ever
-- "fix" this -- see NOTE: [Command log splitting] --, then we can simply
-- flip the expected/unexpected below.
slowOutputBroken :: TestTree
slowOutputBroken = testCase "Slow output is broken" $ do
  results <- run args
  V.verifyExpectedUnexpected results expected unexpected
  where
    scriptPath :: (IsString a) => a
    scriptPath = "./test/functional/Functional/slow_output_broken.sh"

    args =
      withNoConfig
        [ "--console-log-command",
          scriptPath
        ]

    expected =
      [ withCommandPrefix scriptPath "first: ",
        withCommandPrefix scriptPath "second"
      ]

    unexpected =
      [ withCommandPrefix scriptPath "first: second"
      ]
