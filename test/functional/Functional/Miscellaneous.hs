-- | Misc tests
module Functional.Miscellaneous (specs) where

import Functional.Prelude
import Test.Shrun.Verifier (ResultText (MkResultText))
import Test.Shrun.Verifier qualified as V

specs :: TestTree
specs =
  testGroup
    "Miscellaneous"
    [ splitNewlineLogs,
      spaceStderrLogs,
      stripControlAlwaysCmdNames
    ]

splitNewlineLogs :: TestTree
splitNewlineLogs = testCase "Logs with newlines are split" $ do
  results <- fmap MkResultText <$> run args
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

spaceStderrLogs :: TestTree
spaceStderrLogs = testCase "Stderr Log with newlines is spaced" $ do
  results <- fmap MkResultText <$> runExitFailure args
  V.verifyExpectedUnexpected results expected unexpected
  where
    args =
      withNoConfig
        [ "--console-log-command",
          "sleep 1 && echo 'abc\n  def' && exit 1"
        ]

    -- verifying final 'abc\ndef' log is translated to 'abc def' in the final
    -- stderr msg
    expected =
      [ withErrorPrefix "sleep 1 && echo 'abc def' && exit 1" <> "1 second: abc def"
      ]
    unexpected =
      [ withErrorPrefix "sleep 1 && echo 'abc def' && exit 1" <> "1 second: abcdef"
      ]

-- NOTE: This used to be in Examples (subsequently Examples.ConsoleLogging),
-- as it fit in alongside the other tests. However, we prefer those tests to
-- match Configuration.md as closely as possible, to make maintaining the
-- markdown file as easy as possible. Thus we move it here.
stripControlAlwaysCmdNames :: TestTree
stripControlAlwaysCmdNames = testCase "Always strips command names" $ do
  results <- fmap MkResultText <$> run args
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
