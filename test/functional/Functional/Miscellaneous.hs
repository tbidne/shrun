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
      spaceStderrLogs
    ]

splitNewlineLogs :: TestTree
splitNewlineLogs = testCase "Logs with newlines are split" $ do
  results <- fmap MkResultText <$> (readIORef =<< run args)
  V.verifyExpectedUnexpected results expected unexpected
  where
    args =
      withNoConfig
        [ "--cmd-log",
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
  results <- fmap MkResultText <$> (readIORef =<< runExitFailure args)
  V.verifyExpectedUnexpected results expected unexpected
  where
    args =
      withNoConfig
        [ "--cmd-log",
          "sleep 1 && echo 'abc\ndef' && exit 1"
        ]

    -- verifying final 'abc\ndef' log is translated to 'abc def' in the final
    -- stderr msg
    expected =
      [ withErrorPrefix "sleep 1 && echo 'abc def' && exit 1" <> "1 second: abc def"
      ]
    unexpected =
      [ withErrorPrefix "sleep 1 && echo 'abc def' && exit 1" <> "1 second: abcdef"
      ]
