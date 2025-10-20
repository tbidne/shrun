module Unit.Shrun.Configuration.Args.Parsing.Notify (tests) where

import Shrun.Configuration.Args (Args)
import Shrun.Configuration.Data.Notify (NotifyArgs)
import Shrun.Configuration.Data.Notify.Action
  ( NotifyAction (NotifyAll, NotifyCommand, NotifyFinal),
  )
import Shrun.Configuration.Data.Notify.System
  ( NotifySystemP (AppleScript, DBus, NotifySend),
  )
import Shrun.Configuration.Data.Notify.Timeout
  ( NotifyTimeout (NotifyTimeoutNever, NotifyTimeoutSeconds),
  )
import Shrun.Configuration.Data.WithDisabled (WithDisabled (With))
import Unit.Prelude
import Unit.Shrun.Configuration.Args.Parsing.TestUtils qualified as U

tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Args.Parsing.Notify"
    [ notifyActionTests,
      notifySystemTests,
      notifyTimeoutTests
    ]

notifyActionTests :: TestTree
notifyActionTests =
  testGroup
    "--notify-action"
    [ testActionFinal,
      testActionCommand,
      testActionAll,
      testActionDisabled
    ]

testActionFinal :: TestTree
testActionFinal =
  testPropertyNamed desc "testActionFinal"
    $ U.verifyResult argList expected
  where
    desc = "Parses --notify-action final"
    argList = ["--notify-action", "final", "command"]
    expected = updateDefNotifyArgsWD #action NotifyFinal

testActionCommand :: TestTree
testActionCommand =
  testPropertyNamed desc "testActionCommand"
    $ U.verifyResult argList expected
  where
    desc = "Parses --notify-action command"
    argList = ["--notify-action", "command", "command"]
    expected = updateDefNotifyArgsWD #action NotifyCommand

testActionAll :: TestTree
testActionAll =
  testPropertyNamed desc "testActionAll"
    $ U.verifyResult argList expected
  where
    desc = "Parses --notify-action all"
    argList = ["--notify-action", "all", "command"]
    expected = updateDefNotifyArgsWD #action NotifyAll

testActionDisabled :: TestTree
testActionDisabled =
  testPropertyNamed "Parses --notify-action off" "testActionDisabled"
    $ U.verifyResult argList expected
  where
    argList = ["--notify-action", "off", "command"]
    expected = U.disableDefCoreArgs (#notify % #action)

notifySystemTests :: TestTree
notifySystemTests =
  testGroup
    "--notify-system"
    [ testSystemDBus,
      testSystemNotifySend,
      testSystemAppleScript
    ]

testSystemDBus :: TestTree
testSystemDBus =
  testPropertyNamed desc "testSystemDBus"
    $ U.verifyResult argList expected
  where
    desc = "Parses --notify-system dbus"
    argList = ["--notify-system", "dbus", "command"]
    expected = updateDefNotifyArgs #system (DBus ())

testSystemNotifySend :: TestTree
testSystemNotifySend =
  testPropertyNamed desc "testSystemNotifySend"
    $ U.verifyResult argList expected
  where
    desc = "Parses --notify-system notify-send"
    argList = ["--notify-system", "notify-send", "command"]
    expected = updateDefNotifyArgs #system NotifySend

testSystemAppleScript :: TestTree
testSystemAppleScript =
  testPropertyNamed desc "testSystemAppleScript"
    $ U.verifyResult argList expected
  where
    desc = "Parses --notify-system apple-script"
    argList = ["--notify-system", "apple-script", "command"]
    expected = updateDefNotifyArgs #system AppleScript

notifyTimeoutTests :: TestTree
notifyTimeoutTests =
  testGroup
    "--notify-timeout"
    [ testTimeoutSeconds,
      testTimeoutSecondsUnderscores,
      testTimeoutDisabled
    ]

testTimeoutSeconds :: TestTree
testTimeoutSeconds =
  testPropertyNamed desc "testTimeoutSeconds"
    $ U.verifyResult argList expected
  where
    desc = "Parses --notify-timeout 5"
    argList = ["--notify-timeout", "5", "command"]
    expected = updateDefNotifyArgs #timeout (NotifyTimeoutSeconds 5)

testTimeoutSecondsUnderscores :: TestTree
testTimeoutSecondsUnderscores =
  testPropertyNamed desc "testTimeoutSecondsUnderscores"
    $ U.verifyResult argList expected
  where
    desc = "Parses --notify-timeout 5_000"
    argList = ["--notify-timeout", "5_000", "command"]
    expected = updateDefNotifyArgs #timeout (NotifyTimeoutSeconds 5_000)

testTimeoutDisabled :: TestTree
testTimeoutDisabled =
  testPropertyNamed desc "testTimeoutDisabled"
    $ U.verifyResult argList expected
  where
    desc = "Parses --notify-timeout off"
    argList = ["--notify-timeout", "off", "command"]
    expected = updateDefNotifyArgs #timeout NotifyTimeoutNever

updateDefNotifyArgsWD ::
  forall a.
  Lens' NotifyArgs (Maybe (WithDisabled a)) ->
  a ->
  Maybe Args
updateDefNotifyArgsWD l x = (l' ?~ With x) U.defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (Maybe (WithDisabled a))
    l' = _Just % #coreConfig % #notify % l

updateDefNotifyArgs ::
  forall a.
  Lens' NotifyArgs (Maybe a) ->
  a ->
  Maybe Args
updateDefNotifyArgs l x = (l' ?~ x) U.defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (Maybe a)
    l' = _Just % #coreConfig % #notify % l
