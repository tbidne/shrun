{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Tests for Shrun.Args
module Unit.Shrun.Configuration.Args (tests) where

import Options.Applicative (ParserPrefs)
import Options.Applicative qualified as OptApp
import Shrun.Configuration.Args (Args, defaultArgs)
import Shrun.Configuration.Args qualified as Args
import Shrun.Configuration.Args.Parsing (parserInfoArgs)
import Shrun.Configuration.Data.Core (CoreConfigArgs)
import Shrun.Configuration.Data.FileLogging (FileLoggingArgs)
import Shrun.Configuration.Data.Notify (NotifyArgs)
import Shrun.Configuration.Data.WithDisabled (WithDisabled (Disabled, With))
import Shrun.Data.FileMode (FileMode (FileModeAppend, FileModeWrite))
import Shrun.Data.FilePathDefault (FilePathDefault (FPDefault, FPManual))
import Shrun.Data.FileSizeMode
  ( FileSizeMode
      ( FileSizeModeDelete,
        FileSizeModeNothing,
        FileSizeModeWarn
      ),
  )
import Shrun.Data.KeyHide (KeyHide (KeyHideOn))
import Shrun.Data.StripControl
  ( StripControl (StripControlAll, StripControlNone, StripControlSmart),
  )
import Shrun.Data.TimerFormat
  ( TimerFormat
      ( DigitalCompact,
        DigitalFull,
        ProseCompact,
        ProseFull
      ),
  )
import Shrun.Data.Truncation (LineTruncation (Detected, Undetected))
import Shrun.Notify.Types
  ( NotifyAction (NotifyAll, NotifyCommand, NotifyFinal),
    NotifySystem (AppleScript, DBus, NotifySend),
    NotifyTimeout (NotifyTimeoutNever, NotifyTimeoutSeconds),
  )
import Shrun.Utils qualified as U
import Unit.Prelude

-- | Entry point for Shrun.Args specs.
tests :: TestTree
tests =
  testGroup
    "Shrun.Args"
    [ defaultSpec,
      configSpecs,
      timeoutSpecs,
      initSpecs,
      fileLoggingSpecs,
      fileLoggingCmdNameTruncSpecs,
      fileLogModeSpecs,
      fileLogStripControlSpecs,
      fileLogSizeModeSpecs,
      cmdLogReadSizeSpecs,
      commandLoggingSpecs,
      commandDisplaySpecs,
      pollIntervalSpecs,
      timerFormatSpecs,
      stripControlSpecs,
      cmdNameTruncSpecs,
      cmdLineTruncSpecs,
      notifyActionSpecs,
      notifySystemSpecs,
      notifyTimeoutSpecs,
      commandSpecs
    ]

defaultSpec :: TestTree
defaultSpec =
  testGroup
    "Default parsing"
    [parseDefaultArgs]

parseDefaultArgs :: TestTree
parseDefaultArgs = testPropertyNamed desc "parseDefaultArgs" $ do
  let argList = ["command"]
      expected = Just $ defaultArgs ("command" :<|| [])
  verifyResult argList expected
  where
    desc = "Should parse default args"

configSpecs :: TestTree
configSpecs =
  testGroup
    "Config arg parsing"
    [ parseShortConfig,
      parseLongConfig,
      parseNoConfig
    ]

parseShortConfig :: TestTree
parseShortConfig =
  testPropertyNamed "Should parse short config" "parseShortConfig"
    $ verifyResult argList expected
  where
    argList = ["-c./path/config.toml", "command"]
    expected = updateDefArgs #configPath [osp|./path/config.toml|]

parseLongConfig :: TestTree
parseLongConfig =
  testPropertyNamed "Should parse long config" "parseLongConfig"
    $ verifyResult argList expected
  where
    argList = ["--config=./path/config.toml", "command"]
    expected = updateDefArgs #configPath [osp|./path/config.toml|]

parseNoConfig :: TestTree
parseNoConfig =
  testPropertyNamed "Should parse no-config" "parseNoConfig"
    $ verifyResult argList expected
  where
    argList = ["--no-config", "command"]
    expected = disableDefArgs #configPath

timeoutSpecs :: TestTree
timeoutSpecs =
  testGroup
    "Timeout arg parsing"
    [ parseShortTimeout,
      parseLongTimeout,
      parseTimeString,
      parseLongTimeString,
      parseTimeoutWordFail,
      parseNegativeTimeoutFail,
      parseNoTimeout
    ]

parseShortTimeout :: TestTree
parseShortTimeout =
  testPropertyNamed "Should parse short timeout" "parseShortTimeout"
    $ verifyResult argList expected
  where
    argList = ["-t7", "command"]
    expected = updateDefCoreArgs #timeout 7

parseLongTimeout :: TestTree
parseLongTimeout =
  testPropertyNamed "Should parse long timeout" "parseLongTimeout"
    $ verifyResult argList expected
  where
    argList = ["--timeout=7", "command"]
    expected = updateDefCoreArgs #timeout 7

parseTimeString :: TestTree
parseTimeString =
  testPropertyNamed "Should parse time string" "parseTimeString"
    $ verifyResult argList expected
  where
    argList = ["-t2h4s", "command"]
    expected = updateDefCoreArgs #timeout 7204

parseLongTimeString :: TestTree
parseLongTimeString =
  testPropertyNamed "Should parse full time string" "parseLongTimeString"
    $ verifyResult argList expected
  where
    argList = ["--timeout=1d2h3m4s", "command"]
    expected = updateDefCoreArgs #timeout 93784

parseTimeoutWordFail :: TestTree
parseTimeoutWordFail =
  testPropertyNamed "Word should fail" "parseTimeoutWordFail"
    $ verifyFailure argList
  where
    argList = ["--timeout=cat", "command"]

parseNegativeTimeoutFail :: TestTree
parseNegativeTimeoutFail =
  testPropertyNamed "Negative should fail" "parseNegativeTimeoutFail"
    $ verifyFailure argList
  where
    argList = ["--timeout=-7", "command"]

parseNoTimeout :: TestTree
parseNoTimeout =
  testPropertyNamed "Parse --no-timeout" "parseNoTimeout"
    $ verifyResult argList expected
  where
    argList = ["--no-timeout", "command"]
    expected = disableDefCoreArgs #timeout

initSpecs :: TestTree
initSpecs =
  testGroup
    "Init arg parsing"
    [ parseShortInit,
      parseLongInit1,
      parseLongInit2,
      parseNoInit
    ]

parseShortInit :: TestTree
parseShortInit =
  testPropertyNamed "Should parse short init" "parseShortInit"
    $ verifyResult argList expected
  where
    argList = ["-i. ~/.bashrc", "command"]
    expected = updateDefCoreArgs #init ". ~/.bashrc"

parseLongInit1 :: TestTree
parseLongInit1 =
  testPropertyNamed "Should parse long init" "parseLongInit1"
    $ verifyResult argList expected
  where
    argList = ["--init=. ~/.bashrc", "command"]
    expected = updateDefCoreArgs #init ". ~/.bashrc"

parseLongInit2 :: TestTree
parseLongInit2 =
  testPropertyNamed "Should parse long init" "parseLongInit2"
    $ verifyResult argList expected
  where
    argList = ["--init", ". ~/.bashrc", "command"]
    expected = updateDefCoreArgs #init ". ~/.bashrc"

parseNoInit :: TestTree
parseNoInit =
  testPropertyNamed "Parse --no-init" "parseNoInit"
    $ verifyResult argList expected
  where
    argList = ["--no-init", "command"]
    expected = disableDefCoreArgs #init

fileLoggingSpecs :: TestTree
fileLoggingSpecs =
  testGroup
    "FileLog arg parsing"
    [ parseShortFileLogging,
      parseLongFileLogging,
      parseLongDefaultFileLogging,
      parseShortDefaultFileLogging,
      parseShortEmptyFileLoggingFails,
      parseLongEmptyFileLoggingFails,
      parseNoFileLog
    ]

parseShortFileLogging :: TestTree
parseShortFileLogging =
  testPropertyNamed "Should parse filepath with -f" "parseShortFileLogging"
    $ verifyResult argList expected
  where
    argList = ["-flogfile", "command"]
    expected = updateDefFileLogArgs #path (FPManual [osp|logfile|])

parseLongFileLogging :: TestTree
parseLongFileLogging =
  testPropertyNamed desc "parseLongFileLogging"
    $ verifyResult argList expected
  where
    desc = "Should parse filepath with --file-log"
    argList = ["--file-log=logfile", "command"]
    expected = updateDefFileLogArgs #path (FPManual [osp|logfile|])

parseLongDefaultFileLogging :: TestTree
parseLongDefaultFileLogging =
  testPropertyNamed desc "parseLongDefaultFileLogging"
    $ verifyResult argList expected
  where
    desc = "Should parse default --file-log"
    argList = ["--file-log", "default", "command"]
    expected = updateDefFileLogArgs #path FPDefault

parseShortDefaultFileLogging :: TestTree
parseShortDefaultFileLogging =
  testPropertyNamed "Should parse default -f" "parseShortDefaultFileLogging"
    $ verifyResult argList expected
  where
    argList = ["-f", "default", "command"]
    expected = updateDefFileLogArgs #path FPDefault

parseShortEmptyFileLoggingFails :: TestTree
parseShortEmptyFileLoggingFails =
  testPropertyNamed desc "parseShortEmptyFileLoggingFails"
    $ verifyFailure argList
  where
    desc = "Should parse empty -f as failure"
    argList = ["-f", "command"]

parseLongEmptyFileLoggingFails :: TestTree
parseLongEmptyFileLoggingFails =
  testPropertyNamed desc "parseLongEmptyFileLoggingFails"
    $ verifyFailure argList
  where
    desc = "Should parse empty --file-log as failure"
    argList = ["--file-log=", "command"]

parseNoFileLog :: TestTree
parseNoFileLog =
  testPropertyNamed "Parse --no-file-log" "parseNoFileLog"
    $ verifyResult argList expected
  where
    argList = ["--no-file-log", "command"]
    expected = disableDefCoreArgs (#fileLogging % #path)

fileLoggingCmdNameTruncSpecs :: TestTree
fileLoggingCmdNameTruncSpecs =
  testGroup
    "FileLog cmd name trunc arg parsing"
    [ parseFileLogCmdNameTrunc,
      parseNoFileLogCmdNameTrunc
    ]

parseFileLogCmdNameTrunc :: TestTree
parseFileLogCmdNameTrunc =
  testPropertyNamed
    "Should parse --file-log-cmd-name-trunc as command name truncation"
    "parseFileLogCmdNameTrunc"
    $ verifyResult argList expected
  where
    argList = ["--file-log-cmd-name-trunc", "15", "command"]
    expected = updateDefFileLogArgs #cmdNameTrunc 15

parseNoFileLogCmdNameTrunc :: TestTree
parseNoFileLogCmdNameTrunc =
  testPropertyNamed "Parse --no-file-log-cmd-name-trunc" "parseNoFileLogCmdNameTrunc"
    $ verifyResult argList expected
  where
    argList = ["--no-file-log-cmd-name-trunc", "command"]
    expected = disableDefFileLogArgs #cmdNameTrunc

fileLogModeSpecs :: TestTree
fileLogModeSpecs =
  testGroup
    "File log mode"
    [ parseFileLogModeAppend,
      parseFileLogModeWrite,
      parseNoFileLogMode
    ]

parseFileLogModeAppend :: TestTree
parseFileLogModeAppend =
  testPropertyNamed desc "parseFileLogModeAppend"
    $ verifyResult argList expected
  where
    desc = "Should parse --file-log-mode append as FileModeAppend"
    argList = ["--file-log-mode", "append", "command"]
    expected = updateDefFileLogArgs #mode FileModeAppend

parseFileLogModeWrite :: TestTree
parseFileLogModeWrite =
  testPropertyNamed desc "parseFileLogModeWrite"
    $ verifyResult argList expected
  where
    desc = "Should parse --file-log-mode write as FileModeWrite"
    argList = ["--file-log-mode", "write", "command"]
    expected = updateDefFileLogArgs #mode FileModeWrite

parseNoFileLogMode :: TestTree
parseNoFileLogMode =
  testPropertyNamed "Parse --no-file-log-mode" "parseNoFileLogMode"
    $ verifyResult argList expected
  where
    argList = ["--no-file-log-mode", "command"]
    expected = disableDefCoreArgs (#fileLogging % #mode)

fileLogStripControlSpecs :: TestTree
fileLogStripControlSpecs =
  testGroup
    "Strip control arg parsing"
    [ parseFileLogStripControlAll,
      parseFileLogStripControlNone,
      parseFileLogStripControlSmart,
      parseNoFileLogStripControl
    ]

parseFileLogStripControlAll :: TestTree
parseFileLogStripControlAll =
  testPropertyNamed desc "parseFileLogStripControlAll"
    $ verifyResult argList expected
  where
    desc = "Should parse --file-log-strip-control all as StripControlAll"
    argList = ["--file-log-strip-control", "all", "command"]
    expected = updateDefFileLogArgs #stripControl StripControlAll

parseFileLogStripControlNone :: TestTree
parseFileLogStripControlNone =
  testPropertyNamed desc "parseFileLogStripControlNone"
    $ verifyResult argList expected
  where
    desc = "Should parse --file-log-strip-control none as StripControlNone"
    argList = ["--file-log-strip-control", "none", "command"]
    expected = updateDefFileLogArgs #stripControl StripControlNone

parseFileLogStripControlSmart :: TestTree
parseFileLogStripControlSmart =
  testPropertyNamed desc "parseFileLogStripControlSmart"
    $ verifyResult argList expected
  where
    desc = "Should parse --file-log-strip-control smart as StripControlSmart"
    argList = ["--file-log-strip-control", "smart", "command"]
    expected = updateDefFileLogArgs #stripControl StripControlSmart

parseNoFileLogStripControl :: TestTree
parseNoFileLogStripControl =
  testPropertyNamed desc "parseNoFileLogStripControl"
    $ verifyResult argList expected
  where
    desc = "Parse --no-file-log-strip-control"
    argList = ["--no-file-log-strip-control", "command"]
    expected = disableDefCoreArgs (#fileLogging % #stripControl)

fileLogSizeModeSpecs :: TestTree
fileLogSizeModeSpecs =
  testGroup
    "File log size mode parsing"
    [ parseFileLogSizeWarn,
      parseFileLogSizeDelete,
      parseFileLogSizeNothing,
      parseNoFileLogSizeMode
    ]

parseFileLogSizeWarn :: TestTree
parseFileLogSizeWarn =
  testPropertyNamed desc "parseFileLogSizeWarn"
    $ verifyResult argList expected
  where
    desc = "Should parse --file-log-size-mode warn"
    argList = ["--file-log-size-mode", "warn 10 gb", "command"]
    expected =
      updateDefFileLogArgs
        #sizeMode
        (FileSizeModeWarn $ MkBytes 10_000_000_000)

parseFileLogSizeDelete :: TestTree
parseFileLogSizeDelete =
  testPropertyNamed desc "parseFileLogSizeDelete"
    $ verifyResult argList expected
  where
    desc = "Should parse --file-log-size-mode delete"
    argList = ["--file-log-size-mode", "delete 2.4Kilobytes", "command"]
    expected =
      updateDefFileLogArgs
        #sizeMode
        (FileSizeModeDelete $ MkBytes 2_400)

parseFileLogSizeNothing :: TestTree
parseFileLogSizeNothing =
  testPropertyNamed desc "parseFileLogSizeNothing"
    $ verifyResult argList expected
  where
    desc = "Should parse --file-log-size-mode nothing"
    argList = ["--file-log-size-mode", "nothing", "command"]
    expected =
      updateDefFileLogArgs
        #sizeMode
        FileSizeModeNothing

parseNoFileLogSizeMode :: TestTree
parseNoFileLogSizeMode =
  testPropertyNamed "Parse --no-file-log-size-mode" "parseNoFileLogSizeMode"
    $ verifyResult argList expected
  where
    argList = ["--no-file-log-size-mode", "command"]
    expected = disableDefCoreArgs (#fileLogging % #sizeMode)

cmdLogReadSizeSpecs :: TestTree
cmdLogReadSizeSpecs =
  testGroup
    "Command log size arg parsing"
    [ parsecmdLogReadSize,
      parseNocmdLogReadSize
    ]

parsecmdLogReadSize :: TestTree
parsecmdLogReadSize =
  testPropertyNamed
    "Should parse --cmd-log-read-size as command log size"
    "parsecmdLogReadSize"
    $ verifyResult argList expected
  where
    argList = ["--cmd-log-read-size", "2048", "command"]
    expected = updateDefCoreArgs (#cmdLogging % #readSize) (MkBytes 2048)

parseNocmdLogReadSize :: TestTree
parseNocmdLogReadSize =
  testPropertyNamed "Parse --no-cmd-log-read-size" "parseNocmdLogReadSize"
    $ verifyResult argList expected
  where
    argList = ["--no-cmd-log-read-size", "command"]
    expected = disableDefCoreArgs (#cmdLogging % #readSize)

commandLoggingSpecs :: TestTree
commandLoggingSpecs =
  testGroup
    "CmdLog arg parsing"
    [ parseLongCommandLogging,
      parseNoCmdLog
    ]

parseLongCommandLogging :: TestTree
parseLongCommandLogging =
  testPropertyNamed desc "parseLongCommandLogging"
    $ verifyResult argList expected
  where
    desc = "Should parse --console-log-cmd as CmdLogging"
    argList = ["--console-log-cmd", "command"]
    expected = set' (_Just % #coreConfig % #consoleLogging % #cmdLogging) (With ()) defArgs

parseNoCmdLog :: TestTree
parseNoCmdLog =
  testPropertyNamed "Parse --no-console-log-cmd" "parseNoCmdLog"
    $ verifyResult argList expected
  where
    argList = ["--no-console-log-cmd", "command"]
    expected = set' (_Just % #coreConfig % #consoleLogging % #cmdLogging) Disabled defArgs

commandDisplaySpecs :: TestTree
commandDisplaySpecs =
  testGroup
    "KeyHide arg parsing"
    [ parseLongShowKey,
      parseNoKeyHide
    ]

parseLongShowKey :: TestTree
parseLongShowKey =
  testPropertyNamed "Should parse --log-key-hide as KeyHideOff" "parseLongShowKey"
    $ verifyResult argList expected
  where
    argList = ["--log-key-hide", "command"]
    expected = updateDefCoreArgs (#commonLogging % #keyHide) KeyHideOn

parseNoKeyHide :: TestTree
parseNoKeyHide =
  testPropertyNamed "Parse --no-log-key-hide" "parseNoKeyHide"
    $ verifyResult argList expected
  where
    argList = ["--no-log-key-hide", "command"]
    expected = disableDefCoreArgs (#commonLogging % #keyHide)

pollIntervalSpecs :: TestTree
pollIntervalSpecs =
  testGroup
    "PollInterval arg parsing"
    [ parseLongPollInterval,
      parseNoPollInterval
    ]

parseLongPollInterval :: TestTree
parseLongPollInterval =
  testPropertyNamed desc "parseLongPollInterval"
    $ verifyResult argList expected
  where
    desc = "Should parse --cmd-log-poll-interval"
    argList = ["--cmd-log-poll-interval", "1000", "command"]
    expected = updateDefCoreArgs (#cmdLogging % #pollInterval) 1000

parseNoPollInterval :: TestTree
parseNoPollInterval =
  testPropertyNamed "Parse --no-cmd-log-poll-interval" "parseNoPollInterval"
    $ verifyResult argList expected
  where
    argList = ["--no-cmd-log-poll-interval", "command"]
    expected = disableDefCoreArgs (#cmdLogging % #pollInterval)

timerFormatSpecs :: TestTree
timerFormatSpecs =
  testGroup
    "TimerFormat arg parsing"
    [ parseTimerFormatDigitalCompact,
      parseTimerFormatDigitalFull,
      parseTimerFormatProseCompact,
      parseTimerFormatProseFull,
      parseNoTimerFormat
    ]

parseTimerFormatDigitalCompact :: TestTree
parseTimerFormatDigitalCompact =
  testPropertyNamed desc "parseTimerFormatDigitalCompact"
    $ verifyResult argList expected
  where
    desc = "Should parse --log-timer-format digital_compact as DigitalCompact"
    argList = ["--log-timer-format", "digital_compact", "command"]
    expected = updateDefCoreArgs (#commonLogging % #timerFormat) DigitalCompact

parseTimerFormatDigitalFull :: TestTree
parseTimerFormatDigitalFull =
  testPropertyNamed "Should parse --log-timer-format digital_full as DigitalFull" "parseTimerFormatDigitalFull"
    $ verifyResult argList expected
  where
    argList = ["--log-timer-format", "digital_full", "command"]
    expected = updateDefCoreArgs (#commonLogging % #timerFormat) DigitalFull

parseTimerFormatProseCompact :: TestTree
parseTimerFormatProseCompact =
  testPropertyNamed "Should parse --log-timer-format prose_compact as ProseCompact" "parseTimerFormatProseCompact"
    $ verifyResult argList expected
  where
    argList = ["--log-timer-format", "prose_compact", "command"]
    expected = updateDefCoreArgs (#commonLogging % #timerFormat) ProseCompact

parseTimerFormatProseFull :: TestTree
parseTimerFormatProseFull =
  testPropertyNamed desc "parseTimerFormatProseFull"
    $ verifyResult argList expected
  where
    desc = "Should parse --log-timer-format prose_full as ProseFull"
    argList = ["--log-timer-format", "prose_full", "command"]
    expected = updateDefCoreArgs (#commonLogging % #timerFormat) ProseFull

parseNoTimerFormat :: TestTree
parseNoTimerFormat =
  testPropertyNamed "Parse --no-log-timer-format" "parseNoTimerFormat"
    $ verifyResult argList expected
  where
    argList = ["--no-log-timer-format", "command"]
    expected = disableDefCoreArgs (#commonLogging % #timerFormat)

stripControlSpecs :: TestTree
stripControlSpecs =
  testGroup
    "Strip control arg parsing"
    [ parseLongStripControlAll,
      parseLongStripControlNone,
      parseLongStripControlSmart,
      parseNoCmdLogStripControl
    ]

parseLongStripControlAll :: TestTree
parseLongStripControlAll =
  testPropertyNamed desc "parseLongStripControlAll"
    $ verifyResult argList expected
  where
    desc = "Should parse --console-log-strip-control=all as StripControlAll"
    argList = ["--console-log-strip-control=all", "command"]
    expected = updateDefCoreArgs (#consoleLogging % #stripControl) StripControlAll

parseLongStripControlNone :: TestTree
parseLongStripControlNone =
  testPropertyNamed desc "parseLongStripControlNone"
    $ verifyResult argList expected
  where
    desc = "Should parse --console-log-strip-control=None as StripControlNone"
    argList = ["--console-log-strip-control=none", "command"]
    expected = updateDefCoreArgs (#consoleLogging % #stripControl) StripControlNone

parseLongStripControlSmart :: TestTree
parseLongStripControlSmart =
  testPropertyNamed desc "parseLongStripControlSmart"
    $ verifyResult argList expected
  where
    desc = "Should parse --console-log-strip-control=smart as StripControlSmart"
    argList = ["--console-log-strip-control=smart", "command"]
    expected = updateDefCoreArgs (#consoleLogging % #stripControl) StripControlSmart

parseNoCmdLogStripControl :: TestTree
parseNoCmdLogStripControl =
  testPropertyNamed desc "parseNoCmdLogStripControl"
    $ verifyResult argList expected
  where
    desc = "Parse --no-console-log-strip-control"
    argList = ["--no-console-log-strip-control", "command"]
    expected = disableDefCoreArgs (#consoleLogging % #stripControl)

cmdNameTruncSpecs :: TestTree
cmdNameTruncSpecs =
  testGroup
    "Console command name truncation arg parsing"
    [ parseCmdNameTrunc,
      parseNoCmdNameTrunc
    ]

parseCmdNameTrunc :: TestTree
parseCmdNameTrunc =
  testPropertyNamed
    "Should parse --console-log-cmd-name-trunc as command name truncation"
    "parseCmdNameTrunc"
    $ verifyResult argList expected
  where
    argList = ["--console-log-cmd-name-trunc", "15", "command"]
    expected = updateDefCoreArgs (#consoleLogging % #cmdNameTrunc) 15

parseNoCmdNameTrunc :: TestTree
parseNoCmdNameTrunc =
  testPropertyNamed "Parse --no-console-log-cmd-name-trunc" "parseNoCmdNameTrunc"
    $ verifyResult argList expected
  where
    argList = ["--no-console-log-cmd-name-trunc", "command"]
    expected = disableDefCoreArgs (#consoleLogging % #cmdNameTrunc)

cmdLineTruncSpecs :: TestTree
cmdLineTruncSpecs =
  testGroup
    "Command line truncation arg parsing"
    [ parseLongCmdLineTrunc,
      parseDetectCmdLineTrunc,
      parseNoCmdLogLineTrunc
    ]

parseLongCmdLineTrunc :: TestTree
parseLongCmdLineTrunc =
  testPropertyNamed
    "Should parse --console-log-line-trunc as command line truncation"
    "parseLongCmdLineTrunc"
    $ verifyResult argList expected
  where
    argList = ["--console-log-line-trunc", "15", "command"]
    expected = updateDefCoreArgs (#consoleLogging % #lineTrunc) (Undetected 15)

parseDetectCmdLineTrunc :: TestTree
parseDetectCmdLineTrunc =
  testPropertyNamed desc "parseDetectCmdLineTrunc"
    $ verifyResult argList expected
  where
    desc = "Should parse --console-log-line-trunc detect as detect command line truncation"
    argList = ["--console-log-line-trunc", "detect", "command"]
    expected = updateDefCoreArgs (#consoleLogging % #lineTrunc) Detected

parseNoCmdLogLineTrunc :: TestTree
parseNoCmdLogLineTrunc =
  testPropertyNamed "Parse --no-console-log-line-trunc" "parseNoCmdLogLineTrunc"
    $ verifyResult argList expected
  where
    argList = ["--no-console-log-line-trunc", "command"]
    expected = disableDefCoreArgs (#consoleLogging % #lineTrunc)

notifyActionSpecs :: TestTree
notifyActionSpecs =
  testGroup
    "Notify action parsing"
    [ parseNotifyActionFinal,
      parseNotifyActionCommand,
      parseNotifyActionAll,
      parseNoNotifyAction
    ]

parseNotifyActionFinal :: TestTree
parseNotifyActionFinal =
  testPropertyNamed desc "parseNotifyActionFinal"
    $ verifyResult argList expected
  where
    desc = "Should parse --notify-action final"
    argList = ["--notify-action", "final", "command"]
    expected = updateDefNotifyArgs #action NotifyFinal

parseNotifyActionCommand :: TestTree
parseNotifyActionCommand =
  testPropertyNamed desc "parseNotifyActionCommand"
    $ verifyResult argList expected
  where
    desc = "Should parse --notify-action command"
    argList = ["--notify-action", "command", "command"]
    expected = updateDefNotifyArgs #action NotifyCommand

parseNotifyActionAll :: TestTree
parseNotifyActionAll =
  testPropertyNamed desc "parseNotifyActionAll"
    $ verifyResult argList expected
  where
    desc = "Should parse --notify-action all"
    argList = ["--notify-action", "all", "command"]
    expected = updateDefNotifyArgs #action NotifyAll

parseNoNotifyAction :: TestTree
parseNoNotifyAction =
  testPropertyNamed "Parse --no-notify-action" "parseNoNotifyAction"
    $ verifyResult argList expected
  where
    argList = ["--no-notify-action", "command"]
    expected = disableDefCoreArgs (#notify % #action)

notifySystemSpecs :: TestTree
notifySystemSpecs =
  testGroup
    "Notify system parsing"
    [ parseNotifySystemDBus,
      parseNotifySystemNotifySend,
      parseNoNotifySystem,
      parseNotifySystemAppleScript
    ]

parseNotifySystemDBus :: TestTree
parseNotifySystemDBus =
  testPropertyNamed desc "parseNotifySystemDBus"
    $ verifyResult argList expected
  where
    desc = "Should parse --notify-system dbus"
    argList = ["--notify-system", "dbus", "command"]
    expected = updateDefNotifyArgs #system (DBus ())

parseNotifySystemNotifySend :: TestTree
parseNotifySystemNotifySend =
  testPropertyNamed desc "parseNotifySystemNotifySend"
    $ verifyResult argList expected
  where
    desc = "Should parse --notify-system notify-send"
    argList = ["--notify-system", "notify-send", "command"]
    expected = updateDefNotifyArgs #system NotifySend

parseNotifySystemAppleScript :: TestTree
parseNotifySystemAppleScript =
  testPropertyNamed desc "parseNotifySystemAppleScript"
    $ verifyResult argList expected
  where
    desc = "Should parse --notify-system apple-script"
    argList = ["--notify-system", "apple-script", "command"]
    expected = updateDefNotifyArgs #system AppleScript

parseNoNotifySystem :: TestTree
parseNoNotifySystem =
  testPropertyNamed "Parse --no-notify-system" "parseNoNotifySystem"
    $ verifyResult argList expected
  where
    argList = ["--no-notify-system", "command"]
    expected = disableDefCoreArgs (#notify % #system)

notifyTimeoutSpecs :: TestTree
notifyTimeoutSpecs =
  testGroup
    "Notify timeout parsing"
    [ parseNotifyTimeoutSeconds,
      parseNotifyTimeoutNever,
      parseNoNotifyTimeout
    ]

parseNotifyTimeoutSeconds :: TestTree
parseNotifyTimeoutSeconds =
  testPropertyNamed desc "parseNotifyTimeoutSeconds"
    $ verifyResult argList expected
  where
    desc = "Should parse --notify-timeout 5"
    argList = ["--notify-timeout", "5", "command"]
    expected = updateDefNotifyArgs #timeout (NotifyTimeoutSeconds 5)

parseNotifyTimeoutNever :: TestTree
parseNotifyTimeoutNever =
  testPropertyNamed desc "parseNotifyTimeoutNever"
    $ verifyResult argList expected
  where
    desc = "Should parse --notify-timeout never"
    argList = ["--notify-timeout", "never", "command"]
    expected = updateDefNotifyArgs #timeout NotifyTimeoutNever

parseNoNotifyTimeout :: TestTree
parseNoNotifyTimeout =
  testPropertyNamed "Parse --no-notify-timeout" "parseNoNotifyTimeout"
    $ verifyResult argList expected
  where
    argList = ["--no-notify-timeout", "command"]
    expected = disableDefCoreArgs (#notify % #timeout)

commandSpecs :: TestTree
commandSpecs =
  testGroup
    "Command arg parsing"
    [ emptyCommandsFail,
      parseCommands
    ]

emptyCommandsFail :: TestTree
emptyCommandsFail =
  testPropertyNamed "Empty commands fail" "emptyCommandsFail"
    $ verifyFailure []

parseCommands :: TestTree
parseCommands =
  testPropertyNamed "Bare strings parsed as commands" "parseCommands"
    $ verifyResult argList expected
  where
    argList = ["one", "two", "three"]
    expected = ((_Just % #commands) .~ cmds) defArgs
    cmds = U.unsafeListToNESeq ["one", "two", "three"]

verifyResult :: List String -> Maybe Args -> Property
verifyResult argList expected = withTests 1 $ property $ do
  let parseResult = OptApp.execParserPure prefs parserInfoArgs argList

  result <- case parseResult of
    OptApp.Success x -> pure $ Just x
    OptApp.Failure f -> do
      annotate $ fst $ OptApp.renderFailure f "Failed parsing"
      failure
    OptApp.CompletionInvoked _ -> failure

  expected === result

verifyFailure :: List String -> Property
verifyFailure argList = withTests 1 $ property $ do
  let parseResult = OptApp.execParserPure prefs parserInfoArgs argList

  case parseResult of
    OptApp.Success _ -> failure
    OptApp.Failure _ -> pure ()
    OptApp.CompletionInvoked _ -> failure

prefs :: ParserPrefs
prefs = OptApp.prefs mempty

defCommand :: NESeq Text
defCommand = "command" :<|| []

defArgs :: Maybe Args
defArgs = Just $ Args.defaultArgs defCommand

updateDefArgs ::
  forall a.
  Lens' Args (WithDisabled a) ->
  a ->
  Maybe Args
updateDefArgs l x = (l' .~ With x) defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (WithDisabled a)
    l' = _Just % l

disableDefArgs ::
  forall a.
  Lens' Args (WithDisabled a) ->
  Maybe Args
disableDefArgs l = (l' .~ Disabled) defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (WithDisabled a)
    l' = _Just % l

updateDefCoreArgs ::
  forall a.
  Lens' CoreConfigArgs (WithDisabled a) ->
  a ->
  Maybe Args
updateDefCoreArgs l x = (l' .~ With x) defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (WithDisabled a)
    l' = _Just % #coreConfig % l

disableDefCoreArgs ::
  forall a.
  Lens' CoreConfigArgs (WithDisabled a) ->
  Maybe Args
disableDefCoreArgs l = (l' .~ Disabled) defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (WithDisabled a)
    l' = _Just % #coreConfig % l

updateDefFileLogArgs ::
  forall a.
  Lens' FileLoggingArgs (WithDisabled a) ->
  a ->
  Maybe Args
updateDefFileLogArgs l x = (l' .~ With x) defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (WithDisabled a)
    l' = _Just % #coreConfig % #fileLogging % l

disableDefFileLogArgs ::
  forall a.
  Lens' FileLoggingArgs (WithDisabled a) ->
  Maybe Args
disableDefFileLogArgs l = (l' .~ Disabled) defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (WithDisabled a)
    l' = _Just % #coreConfig % #fileLogging % l

updateDefNotifyArgs ::
  forall a.
  Lens' NotifyArgs (WithDisabled a) ->
  a ->
  Maybe Args
updateDefNotifyArgs l x = (l' .~ With x) defArgs
  where
    l' :: AffineTraversal' (Maybe Args) (WithDisabled a)
    l' = _Just % #coreConfig % #notify % l
