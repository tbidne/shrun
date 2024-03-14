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
      -- Having the indexes be 1.3, 2.6, ... rather than integers does not
      -- change anything, but it makes the output slightly clearer.
      withNoConfig
        [ "--cmd-log",
          "--cmd-log-strip-control=all",
          "--cmd-name-trunc=15",
          "sleep 0.5 ; for i in 0.5 1.5 2.5 3.5; do echo \"$i\"; sleep 1; done"
        ]

    -- To test that the logs are correctly streamed, we check that actual
    -- order of logs against the timing logs. This is a bit hacky, but the
    -- sleep of 1.3 makes this more reliable.
    --
    -- Here is why we choose 1.3. We want to avoid a race between Timer logs
    -- (every second) and Command logs, so we want these on different
    -- intervals.
    --
    -- An offset of 1.5 would coincide at t=3, so this is not good.
    -- The first collision with 1.3, on the other hand, is t=11, because the
    -- LCM of 3 and 10 is 30. This is longer than the test runs, so this
    -- should be okay.
    --
    -- An alternative would be to have the timer on the same interval (1 s) and
    -- simply preface the loop with a sleep of 0.5 s.
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
