module Functional.Examples.Notify (tests) where

import DBus.Notify (UrgencyLevel (Normal))
import Functional.Prelude
import Shrun.Configuration.Data.Notify.Timeout
  ( NotifyTimeout
      ( NotifyTimeoutNever,
        NotifyTimeoutSeconds
      ),
  )
import Shrun.Notify.MonadNotify
  ( ShrunNote
      ( MkShrunNote,
        body,
        summary,
        timeout,
        urgency
      ),
  )

-- NOTE: If tests in this module fail, fix then update configuration.md!

tests :: TestTree
tests =
  testGroup
    "Notify"
    [ notifyActionCompleteFinal,
      notifyActionStartOn,
      notifyTimeoutNever
    ]

-- NOTE: There is no DBus test because that requires creating a real DBus
-- connection, as we are running in IO.

notifyActionCompleteFinal :: TestTree
notifyActionCompleteFinal = testCase "Runs --notify-action-complete final" $ do
  results <- runNotes args
  expected @=? results
  where
    args =
      withNoConfig
        [ "--notify-system",
          notifySystemArg,
          "--notify-action-complete",
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

notifyActionStartOn :: TestTree
notifyActionStartOn = testCase "Runs --notify-action-start on" $ do
  results <- runNotes args
  expected @=? results
  where
    args =
      withNoConfig
        [ "--notify-system",
          notifySystemArg,
          "--notify-action-start",
          "on",
          "--edges",
          "sequential",
          "sleep 2",
          "sleep 3"
        ]
    expected =
      [ MkShrunNote
          { summary = "[sleep 3] Started",
            body = "Started after 2 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 10
          },
        MkShrunNote
          { summary = "[sleep 2] Started",
            body = "Started after 0 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 10
          }
      ]

notifyTimeoutNever :: TestTree
notifyTimeoutNever = testCase "Runs --notify-timeout off" $ do
  results <- runNotes args
  expected @=? results
  where
    args =
      withNoConfig
        [ "--notify-system",
          notifySystemArg,
          "--notify-action-complete",
          "all",
          "--notify-timeout",
          "off",
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
