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
      [ "--cmd-log",
        "--cmd-log-strip-control=all",
        "--cmd-name-trunc=15",
        "--no-config",
        "for i in 0 1 2 3; do echo \"$i\"; sleep 1.5; done"
      ]

    -- To test that the logs are correctly streamed, we check that actual
    -- order of logs against the timing logs. This is a bit hacky, but the
    -- sleep of 1.5 makes this fairly reliable.
    expecteds =
      [ withCommandPrefix "for i in 0 1..." "Starting...",
        withCommandPrefix "for i in 0 1..." "0",
        withTimerPrefix "1 second",
        withCommandPrefix "for i in 0 1..." "1",
        withTimerPrefix "2 seconds",
        withCommandPrefix "for i in 0 1..." "2",
        withTimerPrefix "3 seconds",
        withTimerPrefix "4 seconds",
        withCommandPrefix "for i in 0 1..." "3",
        withTimerPrefix "5 seconds",
        withSuccessPrefix "for i in 0 1..." <> "6 seconds",
        withFinishedPrefix "6 seconds"
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
