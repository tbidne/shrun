module Functional.Examples.ConsoleLogging (tests) where

import Functional.Prelude
import Test.Shrun.Verifier qualified as V

-- NOTE: If tests in this module fail, fix then update configuration.md!

tests :: TestTree
tests =
  testGroup
    "ConsoleLogging"
    (multiTestReadStrategy testsParams)
  where
    testsParams :: List ReadStrategyTestParams
    testsParams =
      [ commandLogOn,
        commandLogOnDefault,
        commandLogOff,
        commandNameTruncN,
        commandLogLineTruncN,
        stripControlAll,
        stripControlNone,
        stripControlSmart,
        timerFormatDigitalCompact,
        timerFormatDigitalFull,
        timerFormatProseCompact,
        timerFormatProseFull
      ]

commandLogOn :: ReadStrategyTestParams
commandLogOn =
  ReadStrategyTestParametricSimple
    "Runs commandLog example with --console-log-command"
    run
    args
    (`V.verifyExpected` expected)
  where
    args =
      withNoConfig
        [ "--console-log-command",
          "for i in 1 2; do echo hi; sleep 1; done"
        ]
    expected =
      [ withCommandPrefix "for i in 1 2; do echo hi; sleep 1; done" "hi"
      ]

commandLogOnDefault :: ReadStrategyTestParams
commandLogOnDefault =
  ReadStrategyTestParametricSimple
    "Runs --console-log-command with no output shows default message"
    run
    args
    (`V.verifyExpected` expected)
  where
    args =
      withNoConfig
        [ "--console-log-command",
          "for i in 1 2; do sleep 1; done"
        ]
    expected =
      [ withCommandPrefix "for i in 1 2; do sleep 1; done" "Starting..."
      ]

commandLogOff :: ReadStrategyTestParams
commandLogOff =
  ReadStrategyTestParametricSimple
    "Runs commandLog example without --console-log-command"
    run
    args
    (`V.verifyUnexpected` unexpected)
  where
    args =
      withNoConfig
        [ "for i in 1 2; do echo hi; sleep 1; done"
        ]
    unexpected = [commandPrefix]

commandNameTruncN :: ReadStrategyTestParams
commandNameTruncN =
  ReadStrategyTestParametricSimple
    "Runs --console-log-command-name-trunc 10 example"
    run
    args
    (`V.verifyExpected` expected)
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

commandLogLineTruncN :: ReadStrategyTestParams
commandLogLineTruncN =
  ReadStrategyTestParametricSimple
    "Runs --console-log-line-trunc 80 example"
    run
    args
    (`V.verifyExpected` expected)
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

stripControlAll :: ReadStrategyTestParams
stripControlAll =
  ReadStrategyTestParametricSimple
    "Runs --console-log-strip-control all example"
    run
    args
    (`V.verifyExpected` expected)
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
      [ withCommandPrefix "printf ..." "foo  hello  bye "
      ]

stripControlNone :: ReadStrategyTestParams
stripControlNone =
  ReadStrategyTestParametricSimple
    "Runs --console-log-strip-control none example"
    run
    args
    (`V.verifyExpected` expected)
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

stripControlSmart :: ReadStrategyTestParams
stripControlSmart =
  ReadStrategyTestParametricSimple
    "Runs --console-log-strip-control smart example"
    run
    args
    (`V.verifyExpected` expected)
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
      [ withCommandPrefix "printf ..." "foo \ESC[35m hello  bye "
      ]

timerFormatDigitalCompact :: ReadStrategyTestParams
timerFormatDigitalCompact =
  ReadStrategyTestParametricSimple
    "Runs timer format with digital_compact"
    run
    args
    (`V.verifyExpected` expected)
  where
    args =
      withBaseArgs
        [ "--console-log-timer-format",
          "digital_compact",
          "sleep 2"
        ]
    expected =
      [ withTimerPrefix "01"
      ]

timerFormatDigitalFull :: ReadStrategyTestParams
timerFormatDigitalFull =
  ReadStrategyTestParametricSimple
    "Runs timer format with digital_full"
    run
    args
    (`V.verifyExpected` expected)
  where
    args =
      withBaseArgs
        [ "--console-log-timer-format",
          "digital_full",
          "sleep 2"
        ]
    expected =
      [ withTimerPrefix "00:00:00:01"
      ]

timerFormatProseCompact :: ReadStrategyTestParams
timerFormatProseCompact =
  ReadStrategyTestParametricSimple
    "Runs timer format with prose_compact"
    run
    args
    (`V.verifyExpected` expected)
  where
    args =
      withBaseArgs
        [ "--console-log-timer-format",
          "prose_compact",
          "sleep 2"
        ]
    expected =
      [ withTimerPrefix "1 second"
      ]

timerFormatProseFull :: ReadStrategyTestParams
timerFormatProseFull =
  ReadStrategyTestParametricSimple
    "Runs timer format with prose_full"
    run
    args
    (`V.verifyExpected` expected)
  where
    args =
      withBaseArgs
        [ "--console-log-timer-format",
          "prose_full",
          "sleep 2"
        ]
    expected =
      [ withTimerPrefix "0 days, 0 hours, 0 minutes, 1 second"
      ]
