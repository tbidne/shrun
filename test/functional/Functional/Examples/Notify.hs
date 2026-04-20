module Functional.Examples.Notify (tests) where

import Effects.Notify qualified as Notify
import Functional.Prelude

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
      [ Notify.mkNote "Shrun Finished"
          & Notify.setBody (Just "3 seconds")
          & Notify.setTimeout (Just $ NotifyTimeoutMillis 10_000)
          & Notify.setUrgency (Just NotifyUrgencyNormal)
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
          ";;",
          "sleep 2",
          "sleep 3"
        ]
    expected =
      [ Notify.mkNote "[sleep 3] Started"
          & Notify.setBody (Just "Started after 2 seconds")
          & Notify.setTimeout (Just $ NotifyTimeoutMillis 10_000)
          & Notify.setUrgency (Just NotifyUrgencyNormal),
        Notify.mkNote "[sleep 2] Started"
          & Notify.setBody (Just "Started after 0 seconds")
          & Notify.setTimeout (Just $ NotifyTimeoutMillis 10_000)
          & Notify.setUrgency (Just NotifyUrgencyNormal)
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
      [ Notify.mkNote "Shrun Finished"
          & Notify.setBody (Just "3 seconds")
          & Notify.setTimeout (Just NotifyTimeoutNever)
          & Notify.setUrgency (Just NotifyUrgencyNormal),
        Notify.mkNote "[sleep 3] Finished"
          & Notify.setBody (Just "3 seconds")
          & Notify.setTimeout (Just NotifyTimeoutNever)
          & Notify.setUrgency (Just NotifyUrgencyNormal),
        Notify.mkNote "[sleep 2] Finished"
          & Notify.setBody (Just "2 seconds")
          & Notify.setTimeout (Just NotifyTimeoutNever)
          & Notify.setUrgency (Just NotifyUrgencyNormal)
      ]
