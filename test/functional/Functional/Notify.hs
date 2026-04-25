-- | Functional tests for notifications
module Functional.Notify (specs) where

import Effects.Notify qualified as Notify
import Functional.Prelude

specs :: TestTree
specs = testGroup "Notify" notifyTests

notifyTests :: List TestTree
notifyTests =
  [ notifySystem,
    notifyActionCommand,
    notifyActionAll,
    notifyTimeout5
  ]

-- "Missing tests" i.e.
--
--   - notifySystemDBus
--   - notifyActionFinal
--   - notifyActionTimeoutNever
--
-- are part of the Examples test module

notifySystem :: TestTree
notifySystem = testCase ("Runs --notify-system " <> notifySystemArg) $ do
  results <- runNotes args
  expected @=? results
  where
    args =
      withNoConfig
        [ "--notify-action-complete",
          "command",
          "--notify-system",
          notifySystemArg,
          "sleep 2",
          "sleep 3"
        ]
    expected =
      [ Notify.mkNote "[sleep 3] Finished"
          & Notify.setBody (Just "3 seconds")
          & Notify.setTimeout (Just $ NotifyTimeoutMillis 10_000)
          & Notify.setTitle (Just "Shrun")
          & Notify.setUrgency (Just NotifyUrgencyNormal),
        Notify.mkNote "[sleep 2] Finished"
          & Notify.setBody (Just "2 seconds")
          & Notify.setTimeout (Just $ NotifyTimeoutMillis 10_000)
          & Notify.setTitle (Just "Shrun")
          & Notify.setUrgency (Just NotifyUrgencyNormal)
      ]

notifyActionCommand :: TestTree
notifyActionCommand = testCase "Runs --notify-action-complete command" $ do
  results <- runNotes args
  expected @=? results
  where
    args =
      withNoConfig
        [ "--notify-action-complete",
          "command",
          "--notify-system",
          notifySystemArg,
          "sleep 2",
          "sleep 3"
        ]
    expected =
      [ Notify.mkNote "[sleep 3] Finished"
          & Notify.setBody (Just "3 seconds")
          & Notify.setTimeout (Just $ NotifyTimeoutMillis 10_000)
          & Notify.setTitle (Just "Shrun")
          & Notify.setUrgency (Just NotifyUrgencyNormal),
        Notify.mkNote "[sleep 2] Finished"
          & Notify.setBody (Just "2 seconds")
          & Notify.setTimeout (Just $ NotifyTimeoutMillis 10_000)
          & Notify.setTitle (Just "Shrun")
          & Notify.setUrgency (Just NotifyUrgencyNormal)
      ]

notifyActionAll :: TestTree
notifyActionAll = testCase "Runs --notify-action-complete all" $ do
  results <- runNotes args
  expected @=? results
  where
    args =
      withNoConfig
        [ "--notify-action-complete",
          "all",
          "--notify-system",
          notifySystemArg,
          "sleep 2",
          "sleep 3"
        ]
    expected =
      [ Notify.mkNote "Shrun Finished"
          & Notify.setBody (Just "3 seconds")
          & Notify.setTimeout (Just $ NotifyTimeoutMillis 10_000)
          & Notify.setTitle (Just "Shrun")
          & Notify.setUrgency (Just NotifyUrgencyNormal),
        Notify.mkNote "[sleep 3] Finished"
          & Notify.setBody (Just "3 seconds")
          & Notify.setTimeout (Just $ NotifyTimeoutMillis 10_000)
          & Notify.setTitle (Just "Shrun")
          & Notify.setUrgency (Just NotifyUrgencyNormal),
        Notify.mkNote "[sleep 2] Finished"
          & Notify.setBody (Just "2 seconds")
          & Notify.setTimeout (Just $ NotifyTimeoutMillis 10_000)
          & Notify.setTitle (Just "Shrun")
          & Notify.setUrgency (Just NotifyUrgencyNormal)
      ]

notifyTimeout5 :: TestTree
notifyTimeout5 = testCase "Runs --notify-timeout 5" $ do
  results <- runNotes args
  expected @=? results
  where
    args =
      withNoConfig
        [ "--notify-action-complete",
          "all",
          "--notify-system",
          notifySystemArg,
          "--notify-timeout",
          "0m5s",
          "sleep 2",
          "sleep 3"
        ]
    expected =
      [ Notify.mkNote "Shrun Finished"
          & Notify.setBody (Just "3 seconds")
          & Notify.setTimeout (Just $ NotifyTimeoutMillis 5_000)
          & Notify.setTitle (Just "Shrun")
          & Notify.setUrgency (Just NotifyUrgencyNormal),
        Notify.mkNote "[sleep 3] Finished"
          & Notify.setBody (Just "3 seconds")
          & Notify.setTimeout (Just $ NotifyTimeoutMillis 5_000)
          & Notify.setTitle (Just "Shrun")
          & Notify.setUrgency (Just NotifyUrgencyNormal),
        Notify.mkNote "[sleep 2] Finished"
          & Notify.setBody (Just "2 seconds")
          & Notify.setTimeout (Just $ NotifyTimeoutMillis 5_000)
          & Notify.setTitle (Just "Shrun")
          & Notify.setUrgency (Just NotifyUrgencyNormal)
      ]
