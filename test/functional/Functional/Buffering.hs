-- | Functional test for command log buffering.
module Functional.Buffering (specs) where

import Data.List qualified as L
import Data.Text qualified as T
import Functional.Prelude

specs :: TestTree
specs =
  testGroup
    "Buffering"
    [ logsNoBuffer
    ]

-- We want to ensure that command logs are correctly not buffered i.e.
-- they are streamed, not dumped at the end.
logsNoBuffer :: TestTree
logsNoBuffer =
  testCase "Command logs should not buffer" $ do
    results <- L.reverse <$> (readIORef =<< run args)

    assertLogsEq expecteds results
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
        [ "--cmd-log",
          "--cmd-log-strip-control=all",
          "--cmd-name-trunc=15",
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
    expecteds =
      [ withCommandPrefix cmdPrefix "Starting...",
        withCommandPrefix cmdPrefix "0.5",
        withTimerPrefix "1 second",
        withCommandPrefix cmdPrefix "1.5",
        withTimerPrefix "2 seconds",
        withCommandPrefix cmdPrefix "2.5",
        withTimerPrefix "3 seconds",
        withCommandPrefix cmdPrefix "3.5",
        withTimerPrefix "4 seconds",
        withSuccessPrefix cmdPrefix <> "4 seconds",
        withFinishedPrefix "4 seconds"
      ]

assertLogsEq :: List Text -> List Text -> IO ()
assertLogsEq expecteds results = case go expecteds results of
  Nothing -> pure ()
  Just errMsg -> assertFailure $ unpack errMsg
  where
    go :: List Text -> List Text -> Maybe Text
    go [] [] = Nothing
    go (_ : _) [] =
      Just
        $ mconcat
          [ "Num expected > results.\n\nExpected:\n",
            prettyList expecteds,
            "\n\nResults:\n",
            prettyList results
          ]
    go [] (_ : _) =
      Just
        $ mconcat
          [ "Num results > expected.\n\nExpected:\n",
            prettyList expecteds,
            "\n\nResults:\n",
            prettyList results
          ]
    go (e : es) (r : rs) =
      case logEq e r of
        Nothing -> go es rs
        Just errMsg ->
          Just
            $ mconcat
              [ errMsg,
                "\n\nAll expecteds:\n",
                prettyList expecteds,
                "\n\nAll results:\n",
                prettyList results
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
