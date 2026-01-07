module Unit.Shrun.Configuration.Args.Parsing.Notify (tests) where

import Shrun.Configuration.Args (Args)
import Shrun.Configuration.Data.Notify (NotifyArgs)
import Shrun.Configuration.Data.Notify.Action
  ( NotifyActionComplete
      ( NotifyActionCompleteAll,
        NotifyActionCompleteCommand,
        NotifyActionCompleteFinal
      ),
    NotifyActionStartSwitch (MkNotifyActionStartSwitch),
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
    [ notifyActionCompleteTests,
      notifyActionStartTests,
      notifySystemTests,
      notifyTimeoutTests
    ]

notifyActionCompleteTests :: TestTree
notifyActionCompleteTests =
  testGroup
    "--notify-action-complete"
    [ testActionCompleteFinal,
      testActionCompleteCommand,
      testActionCompleteAll,
      testActionCompleteDisabled
    ]

testActionCompleteFinal :: TestTree
testActionCompleteFinal =
  testPropertyNamed desc "testActionCompleteFinal"
    $ U.verifyResult argList expected
  where
    desc = "Parses --notify-action-complete final"
    argList = ["--notify-action-complete", "final", "command"]
    expected = updateDefNotifyArgsWD (#actions % #complete) NotifyActionCompleteFinal

testActionCompleteCommand :: TestTree
testActionCompleteCommand =
  testPropertyNamed desc "testActionCompleteCommand"
    $ U.verifyResult argList expected
  where
    desc = "Parses --notify-action-complete command"
    argList = ["--notify-action-complete", "command", "command"]
    expected = updateDefNotifyArgsWD (#actions % #complete) NotifyActionCompleteCommand

testActionCompleteAll :: TestTree
testActionCompleteAll =
  testPropertyNamed desc "testActionCompleteAll"
    $ U.verifyResult argList expected
  where
    desc = "Parses --notify-action-complete all"
    argList = ["--notify-action-complete", "all", "command"]
    expected = updateDefNotifyArgsWD (#actions % #complete) NotifyActionCompleteAll

testActionCompleteDisabled :: TestTree
testActionCompleteDisabled =
  testPropertyNamed "Parses --notify-action-complete off" "testActionCompleteDisabled"
    $ U.verifyResult argList expected
  where
    argList = ["--notify-action-complete", "off", "command"]
    expected = U.disableDefCoreArgs (#notify % #actions % #complete)

notifyActionStartTests :: TestTree
notifyActionStartTests =
  testGroup
    "--notify-action-start"
    [ testActionStartOn,
      testActionStartOff
    ]

testActionStartOn :: TestTree
testActionStartOn =
  testPropertyNamed desc "testActionStartOn"
    $ U.verifyResult argList expected
  where
    desc = "Parses --notify-action-start on"
    argList = ["--notify-action-start", "on", "command"]
    expected = updateDefNotifyArgs (#actions % #start) (MkNotifyActionStartSwitch True)

testActionStartOff :: TestTree
testActionStartOff =
  testPropertyNamed desc "testActionStartOff"
    $ U.verifyResult argList expected
  where
    desc = "Parses --notify-action-start off"
    argList = ["--notify-action-start", "off", "command"]
    expected = updateDefNotifyArgs (#actions % #start) (MkNotifyActionStartSwitch False)

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
      testTimeoutString,
      testTimeoutWordFail,
      testTimeoutNegativeFail,
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

testTimeoutString :: TestTree
testTimeoutString =
  testPropertyNamed "Parses --notify-timeout 1d2h3m4s" "testTimeoutString"
    $ U.verifyResult argList expected
  where
    argList = ["--notify-timeout", "1d2h3m4s", "command"]
    expected = updateDefNotifyArgs #timeout (NotifyTimeoutSeconds 93_784)

testTimeoutWordFail :: TestTree
testTimeoutWordFail =
  testPropertyNamed "Parses --notify-timeout cat failure" "testTimeoutWordFail"
    $ U.verifyFailure argList
  where
    argList = ["--notify-timeout", "cat", "command"]

testTimeoutNegativeFail :: TestTree
testTimeoutNegativeFail =
  testPropertyNamed "Parses --notify-timeout -7" "testTimeoutNegativeFail"
    $ U.verifyFailure argList
  where
    argList = ["--notify-timeout", "-7", "command"]

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
