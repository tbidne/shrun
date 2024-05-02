module Unit.Shrun.Configuration.Args.Parsing.Notify (tests) where

import Shrun.Configuration.Args (Args)
import Shrun.Configuration.Data.Notify (NotifyArgs)
import Shrun.Configuration.Data.WithDisabled (WithDisabled (With))
import Shrun.Notify.Types
  ( NotifyAction (NotifyAll, NotifyCommand, NotifyFinal),
    NotifySystemP (AppleScript, DBus, NotifySend),
    NotifyTimeout (NotifyTimeoutNever, NotifyTimeoutSeconds),
  )
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
      testNoAction
    ]

testActionFinal :: TestTree
testActionFinal =
  testPropertyNamed desc "testActionFinal"
    $ U.verifyResult argList expected
  where
    desc = "Parses --notify-action final"
    argList = ["--notify-action", "final", "command"]
    expected = updateDefNotifyArgs #action NotifyFinal

testActionCommand :: TestTree
testActionCommand =
  testPropertyNamed desc "testActionCommand"
    $ U.verifyResult argList expected
  where
    desc = "Parses --notify-action command"
    argList = ["--notify-action", "command", "command"]
    expected = updateDefNotifyArgs #action NotifyCommand

testActionAll :: TestTree
testActionAll =
  testPropertyNamed desc "testActionAll"
    $ U.verifyResult argList expected
  where
    desc = "Parses --notify-action all"
    argList = ["--notify-action", "all", "command"]
    expected = updateDefNotifyArgs #action NotifyAll

testNoAction :: TestTree
testNoAction =
  testPropertyNamed "Parses --no-notify-action" "testNoAction"
    $ U.verifyResult argList expected
  where
    argList = ["--no-notify-action", "command"]
    expected = U.disableDefCoreArgs (#notify % #action)

notifySystemTests :: TestTree
notifySystemTests =
  testGroup
    "--notify-system"
    [ testSystemDBus,
      testSystemNotifySend,
      testNoSystem,
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

testNoSystem :: TestTree
testNoSystem =
  testPropertyNamed "Parses --no-notify-system" "testNoSystem"
    $ U.verifyResult argList expected
  where
    argList = ["--no-notify-system", "command"]
    expected = U.disableDefCoreArgs (#notify % #system)

notifyTimeoutTests :: TestTree
notifyTimeoutTests =
  testGroup
    "--notify-timeout"
    [ testTimeoutSeconds,
      testTimeoutNever,
      testNoTimeout
    ]

testTimeoutSeconds :: TestTree
testTimeoutSeconds =
  testPropertyNamed desc "testTimeoutSeconds"
    $ U.verifyResult argList expected
  where
    desc = "Parses --notify-timeout 5"
    argList = ["--notify-timeout", "5", "command"]
    expected = updateDefNotifyArgs #timeout (NotifyTimeoutSeconds 5)

testTimeoutNever :: TestTree
testTimeoutNever =
  testPropertyNamed desc "testTimeoutNever"
    $ U.verifyResult argList expected
  where
    desc = "Parses --notify-timeout never"
    argList = ["--notify-timeout", "never", "command"]
    expected = updateDefNotifyArgs #timeout NotifyTimeoutNever

testNoTimeout :: TestTree
testNoTimeout =
  testPropertyNamed "Parses --no-notify-timeout" "testNoTimeout"
    $ U.verifyResult argList expected
  where
    argList = ["--no-notify-timeout", "command"]
    expected = U.disableDefCoreArgs (#notify % #timeout)

updateDefNotifyArgs ::
  forall a.
  Lens' NotifyArgs (WithDisabled a) ->
  a ->
  Maybe Args
updateDefNotifyArgs l x = (l' .~ With x) U.defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (WithDisabled a)
    l' = _Just % #coreConfig % #notify % l
