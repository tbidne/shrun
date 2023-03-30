{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Functional tests for notifications
module Functional.Notify (specs) where

import DBus.Notify (UrgencyLevel (..))
import Functional.Prelude
import Shrun.Notify.MonadNotify (ShrunNote (..))
import Shrun.Notify.Types (NotifyTimeout (..))

specs :: TestTree
specs = testGroup "Notify" notifyTests

notifyTests :: [TestTree]
#if OSX
notifyTests = []
#else
notifyTests =
  [ notifySystemNotifySend,
    notifyActionNone,
    notifyActionCommand,
    notifyTimeout5
  ]

-- "Missing tests" i.e.
--
--   - notifySystemDBus
--   - notifyActionFinal
--   - notifyActionTimeoutNever
--
-- are part of the Readme module

notifySystemNotifySend :: TestTree
notifySystemNotifySend = testCase "Runs --notify-system notify-send" $ do
  results <- readIORef =<< runNotes args
  expected @=? results
  where
    args =
      withNoConfig
        [ "--notify-action",
          "command",
          "--notify-system",
          "notify-send",
          "sleep 2",
          "sleep 3"
        ]
    expected =
      [ MkShrunNote
          { summary = "Shrun Finished",
            body = "3 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 10
          },
        MkShrunNote
          { summary = "[sleep 3]  Finished",
            body = "3 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 10
          },
        MkShrunNote
          { summary = "[sleep 2]  Finished",
            body = "2 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 10
          }
      ]

notifyActionNone :: TestTree
notifyActionNone = testCase "Runs --notify-action none" $ do
  results <- readIORef =<< runNotes args
  expected @=? results
  where
    args =
      withNoConfig
        [ "--notify-system",
          "notify-send",
          "--notify-action",
          "none",
          "sleep 2",
          "sleep 3"
        ]
    expected = []

notifyActionCommand :: TestTree
notifyActionCommand = testCase "Runs --notify-action command" $ do
  results <- readIORef =<< runNotes args
  expected @=? results
  where
    args =
      withNoConfig
        [ "--notify-action",
          "command",
          "--notify-system",
          "notify-send",
          "sleep 2",
          "sleep 3"
        ]
    expected =
      [ MkShrunNote
          { summary = "Shrun Finished",
            body = "3 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 10
          },
        MkShrunNote
          { summary = "[sleep 3]  Finished",
            body = "3 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 10
          },
        MkShrunNote
          { summary = "[sleep 2]  Finished",
            body = "2 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 10
          }
      ]

notifyTimeout5 :: TestTree
notifyTimeout5 = testCase "Runs --notify-timeout 5" $ do
  results <- readIORef =<< runNotes args
  expected @=? results
  where
    args =
      withNoConfig
        [ "--notify-action",
          "command",
          "--notify-system",
          "notify-send",
          "--notify-timeout",
          "5",
          "sleep 2",
          "sleep 3"
        ]
    expected =
      [ MkShrunNote
          { summary = "Shrun Finished",
            body = "3 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 5
          },
        MkShrunNote
          { summary = "[sleep 3]  Finished",
            body = "3 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 5
          },
        MkShrunNote
          { summary = "[sleep 2]  Finished",
            body = "2 seconds",
            urgency = Normal,
            timeout = NotifyTimeoutSeconds 5
          }
      ]
#endif
