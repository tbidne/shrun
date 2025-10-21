-- | Functional test for command log buffering.
module Functional.Buffering (specs) where

import Functional.Prelude
import Test.Shrun.Verifier (ExpectedText (MkExpectedText))
import Test.Shrun.Verifier qualified as V

specs :: TestTree
specs =
  testGroup
    "Buffering"
    (multiTestReadStrategy testsParams)
  where
    testsParams :: List ReadStrategyTestParams
    testsParams = [logsNoBuffer]

-- We want to ensure that command logs are correctly not buffered i.e.
-- they are streamed, not dumped at the end.
logsNoBuffer :: ReadStrategyTestParams
logsNoBuffer =
  ReadStrategyTestParametricSimple
    "Command logs should not buffer"
    run
    args
    ( \results -> do
        V.verifyExpectedOrder results expectedOrdered1
        V.verifyExpectedOrder results expectedOrdered2
        V.verifyExpected results (MkExpectedText <$> allExpected)
    )
  where
    -- NOTE: [Bash brace loop interpolation]
    --
    -- The bash loop brace syntax "for i in {0..3}" does not seem to
    -- interpolate correctly with nix or on CI. The brace is interpreted
    -- literally i.e. {0..3}. The manual listing of indices appears to work,
    -- however
    args =
      -- Having the indexes be 0.5, 1.5, ... rather than integers does not
      -- change anything, but it makes the output slightly clearer.
      withNoConfig
        [ "--console-log-command",
          "on",
          "--console-log-strip-control=all",
          "--console-log-command-name-trunc=15",
          "sleep 0.5 ; for i in 0.5 1.5 2.5 3.5 4.5; do echo \"$i\"; sleep 1; done"
        ]

    -- To test that the logs are correctly streamed, we check that actual
    -- order of logs against the timing logs. This is a bit hacky, but the
    -- offset of 0.5 makes this more reliable.
    --
    -- Basically, we do not want a race between Timer logs (every 1s, starting
    -- at 100_000 ms) and Command logs. We tried making the intervals
    -- different (e.g. Command every 1.3s), but that proved flaky as it
    -- gets close enough to the Timer logs (e.g. Timer 4s vs. Command 3.9s)
    -- for a race.
    --
    -- Having the same interval yet an offset make this more reliable.
    cmdPrefix :: (IsString a) => a
    cmdPrefix = "sleep 0.5 ; ..."

    -- Unfortunately osx is sometimes pretty slow on CI, leaving this
    -- vulnerable to timing issues. Hence we try to be pretty lenient and
    -- verify that some logs appear before others e.g. all commands logs
    -- <= 1.5s should appear before timer >= 3, and all timer logs <= 1 second
    -- should appear before all command logs >= 2.5.
    --
    -- This should be enough to verify that we are in fact streaming logs.
    expectedOrdered1 :: (IsString a, Semigroup a) => List a
    expectedOrdered1 =
      [ withCommandPrefix cmdPrefix "Starting...",
        withCommandPrefix cmdPrefix "0.5",
        withCommandPrefix cmdPrefix "1.5",
        withCommandPrefix cmdPrefix "2.5",
        withTimerPrefix "4 seconds",
        withTimerPrefix "5 seconds"
      ]

    expectedOrdered2 :: (IsString a, Semigroup a) => List a
    expectedOrdered2 =
      [ withTimerPrefix "1 second",
        withTimerPrefix "2 seconds",
        withCommandPrefix cmdPrefix "3.5",
        withCommandPrefix cmdPrefix "4.5"
      ]

    -- The order of these last few messages is flaky on OSX, so just look for
    -- existence.
    allExpected =
      expectedOrdered1
        ++ expectedOrdered2
        ++ [ withSuccessPrefix cmdPrefix,
             withFinishedPrefix ""
           ]
