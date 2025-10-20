-- | Functional test for command log buffering.
module Functional.Buffering (specs) where

import Data.Text qualified as T
import Functional.Prelude
import Test.Shrun.Verifier (ExpectedText (MkExpectedText), ResultText)
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
        assertLogsEq expectedOrdered results
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
          "sleep 0.5 ; for i in 0.5 1.5 2.5 3.5; do echo \"$i\"; sleep 1; done"
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
    cmdPrefix = "sleep 0.5 ; ..."

    expectedOrdered =
      [ withCommandPrefix cmdPrefix "Starting...",
        withCommandPrefix cmdPrefix "0.5",
        withTimerPrefix "1 second",
        withCommandPrefix cmdPrefix "1.5",
        withTimerPrefix "2 seconds",
        withCommandPrefix cmdPrefix "2.5",
        withTimerPrefix "3 seconds",
        withCommandPrefix cmdPrefix "3.5",
        withTimerPrefix "4 seconds"
      ]

    -- The order of these last few messages is flaky on OSX, so just look for
    -- existence.
    allExpected =
      expectedOrdered
        ++ [ withSuccessPrefix cmdPrefix,
             withFinishedPrefix ""
           ]

assertLogsEq :: List Text -> List ResultText -> IO ()
assertLogsEq expectedOrdered results = case go expectedOrdered results' of
  Nothing -> pure ()
  Just errMsg -> assertFailure $ unpack errMsg
  where
    results' = (.unResultText) <$> results

    go :: List Text -> List Text -> Maybe Text
    go [] [] = Nothing
    go (_ : _) [] =
      Just
        $ mconcat
          [ "Num expected > results.\n\nExpected:\n",
            prettyList expectedOrdered,
            "\n\nResults:\n",
            prettyList results'
          ]
    -- Having leftover results is fine, since the last few messages are quite
    -- non-deterministic, so we don't bother.
    go [] (_ : _) = Nothing
    go (e : es) (r : rs) =
      case logEq e r of
        Nothing -> go es rs
        Just errMsg ->
          Just
            $ mconcat
              [ errMsg,
                "\n\nAll expected:\n",
                prettyList expectedOrdered,
                "\n\nAll results:\n",
                prettyList results'
              ]

    prettyList :: List Text -> Text
    prettyList [] = ""
    prettyList (x : xs) = x <> "\n" <> prettyList xs

logEq :: Text -> Text -> Maybe Text
logEq expected result =
  if expected `T.isInfixOf` result
    then Nothing
    else Just errMsg
  where
    errMsg =
      mconcat
        [ "Expected '",
          expected,
          "' to be infix of '",
          result,
          "'"
        ]
