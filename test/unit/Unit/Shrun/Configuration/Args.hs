{-# LANGUAGE OverloadedLists #-}

-- | Tests for Shrun.Args
module Unit.Shrun.Configuration.Args (tests) where

import Data.Bytes (Bytes (..))
import Options.Applicative (ParserPrefs)
import Options.Applicative qualified as OptApp
import Shrun.Configuration.Args (Args (..), FileMode (..), FileSizeMode (..))
import Shrun.Configuration.Args qualified as Args
import Shrun.Configuration.Env.Types
  ( CmdDisplay (..),
    LineTruncation (..),
    StripControl (..),
  )
import Shrun.Data.FilePathDefault (FilePathDefault (..))
import Shrun.Notify.Types (NotifyAction (..), NotifySystem (..), NotifyTimeout (..))
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
      fileLogModeSpecs,
      fileLogStripControlSpecs,
      fileLogSizeModeSpecs,
      commandLoggingSpecs,
      commandDisplaySpecs,
      pollIntervalSpecs,
      stripControlSpecs,
      cmdNameTruncSpecs,
      cmdLineTruncSpecs,
      notifySystemSpecs,
      notifyActionSpecs,
      notifyTimeoutSpecs,
      commandSpecs
    ]

defaultSpec :: TestTree
defaultSpec =
  testGroup
    "Default parsing"
    [parseDefaultArgs]

parseDefaultArgs :: TestTree
parseDefaultArgs = testCase "Should parse default args" $ do
  let argList = ["command"]
      expected =
        Just $
          MkArgs
            { configPath = Nothing,
              noConfig = False,
              timeout = Nothing,
              init = Nothing,
              cmdDisplay = Nothing,
              pollInterval = Nothing,
              cmdNameTrunc = Nothing,
              cmdLogging = Nothing,
              cmdLogStripControl = Nothing,
              cmdLogLineTrunc = Nothing,
              fileLogging = Nothing,
              fileLogMode = Nothing,
              fileLogStripControl = Nothing,
              fileLogSizeMode = Nothing,
              notifySystem = Nothing,
              notifyAction = Nothing,
              notifyTimeout = Nothing,
              commands = "command" :<|| []
            }
  verifyResult argList expected

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
  testCase "Should parse short config" $
    verifyResult argList expected
  where
    argList = ["-c./path/config.toml", "command"]
    expected = updateDefArgs #configPath "./path/config.toml"

parseLongConfig :: TestTree
parseLongConfig =
  testCase "Should parse long config" $
    verifyResult argList expected
  where
    argList = ["--config=./path/config.toml", "command"]
    expected = updateDefArgs #configPath "./path/config.toml"

parseNoConfig :: TestTree
parseNoConfig =
  testCase "Should parse no-config" $
    verifyResult argList expected
  where
    argList = ["--no-config", "command"]
    expected = ((_Just % #noConfig) .~ True) defArgs

timeoutSpecs :: TestTree
timeoutSpecs =
  testGroup
    "Timeout arg parsing"
    [ parseShortTimeout,
      parseLongTimeout,
      parseTimeString,
      parseLongTimeString,
      parseTimeoutWordFail,
      parseNegativeTimeoutFail
    ]

parseShortTimeout :: TestTree
parseShortTimeout =
  testCase "Should parse short timeout" $
    verifyResult argList expected
  where
    argList = ["-t7", "command"]
    expected = updateDefArgs #timeout 7

parseLongTimeout :: TestTree
parseLongTimeout =
  testCase "Should parse long timeout" $
    verifyResult argList expected
  where
    argList = ["--timeout=7", "command"]
    expected = updateDefArgs #timeout 7

parseTimeString :: TestTree
parseTimeString =
  testCase "Should parse time string" $
    verifyResult argList expected
  where
    argList = ["-t2h4s", "command"]
    expected = updateDefArgs #timeout 7204

parseLongTimeString :: TestTree
parseLongTimeString =
  testCase "Should parse full time string" $
    verifyResult argList expected
  where
    argList = ["--timeout=1d2h3m4s", "command"]
    expected = updateDefArgs #timeout 93784

parseTimeoutWordFail :: TestTree
parseTimeoutWordFail =
  testCase "Word should fail" $
    verifyResult argList Nothing
  where
    argList = ["--timeout=cat", "command"]

parseNegativeTimeoutFail :: TestTree
parseNegativeTimeoutFail =
  testCase "Negative should fail" $
    verifyResult argList Nothing
  where
    argList = ["--timeout=-7", "command"]

initSpecs :: TestTree
initSpecs =
  testGroup
    "Init arg parsing"
    [ parseShortInit,
      parseLongInit1,
      parseLongInit2
    ]

parseShortInit :: TestTree
parseShortInit =
  testCase "Should parse short init" $
    verifyResult argList expected
  where
    argList = ["-i. ~/.bashrc", "command"]
    expected = updateDefArgs #init ". ~/.bashrc"

parseLongInit1 :: TestTree
parseLongInit1 =
  testCase "Should parse long init" $
    verifyResult argList expected
  where
    argList = ["--init=. ~/.bashrc", "command"]
    expected = updateDefArgs #init ". ~/.bashrc"

parseLongInit2 :: TestTree
parseLongInit2 =
  testCase "Should parse long init" $
    verifyResult argList expected
  where
    argList = ["--init", ". ~/.bashrc", "command"]
    expected = updateDefArgs #init ". ~/.bashrc"

fileLoggingSpecs :: TestTree
fileLoggingSpecs =
  testGroup
    "FileLogging arg parsing"
    [ parseShortFileLogging,
      parseLongFileLogging,
      parseLongDefaultFileLogging,
      parseShortDefaultFileLogging,
      parseShortEmptyFileLoggingFails,
      parseLongEmptyFileLoggingFails
    ]

parseShortFileLogging :: TestTree
parseShortFileLogging =
  testCase "Should parse filepath with -f" $
    verifyResult argList expected
  where
    argList = ["-flogfile", "command"]
    expected = updateDefArgs #fileLogging (FPManual "logfile")

parseLongFileLogging :: TestTree
parseLongFileLogging =
  testCase "Should parse filepath with --file-log" $
    verifyResult argList expected
  where
    argList = ["--file-log=logfile", "command"]
    expected = updateDefArgs #fileLogging (FPManual "logfile")

parseLongDefaultFileLogging :: TestTree
parseLongDefaultFileLogging =
  testCase "Should parse default --file-log" $
    verifyResult argList expected
  where
    argList = ["--file-log", "default", "command"]
    expected = updateDefArgs #fileLogging FPDefault

parseShortDefaultFileLogging :: TestTree
parseShortDefaultFileLogging =
  testCase "Should parse default -f" $
    verifyResult argList expected
  where
    argList = ["-f", "default", "command"]
    expected = updateDefArgs #fileLogging FPDefault

parseShortEmptyFileLoggingFails :: TestTree
parseShortEmptyFileLoggingFails =
  testCase "Should parse empty -f as failure" $
    verifyResult argList Nothing
  where
    argList = ["-f", "command"]

parseLongEmptyFileLoggingFails :: TestTree
parseLongEmptyFileLoggingFails =
  testCase desc $
    verifyResult argList Nothing
  where
    desc = "Should parse empty --file-log as failure"
    argList = ["--file-log=", "command"]

fileLogModeSpecs :: TestTree
fileLogModeSpecs =
  testGroup
    "File log mode"
    [ parseFileLogModeAppend,
      parseFileLogModeWrite
    ]

parseFileLogModeAppend :: TestTree
parseFileLogModeAppend =
  testCase desc $
    verifyResult argList expected
  where
    desc = "Should parse --file-log-mode append as FileModeAppend"
    argList = ["--file-log-mode", "append", "command"]
    expected = updateDefArgs #fileLogMode FileModeAppend

parseFileLogModeWrite :: TestTree
parseFileLogModeWrite =
  testCase desc $
    verifyResult argList expected
  where
    desc = "Should parse --file-log-mode write as FileModeWrite"
    argList = ["--file-log-mode", "write", "command"]
    expected = updateDefArgs #fileLogMode FileModeWrite

fileLogStripControlSpecs :: TestTree
fileLogStripControlSpecs =
  testGroup
    "Strip control arg parsing"
    [ parseFileLogStripControlAll,
      parseFileLogStripControlNone,
      parseFileLogStripControlSmart
    ]

parseFileLogStripControlAll :: TestTree
parseFileLogStripControlAll =
  testCase desc $
    verifyResult argList expected
  where
    desc = "Should parse --file-log-strip-control all as StripControlAll"
    argList = ["--file-log-strip-control", "all", "command"]
    expected = updateDefArgs #fileLogStripControl StripControlAll

parseFileLogStripControlNone :: TestTree
parseFileLogStripControlNone =
  testCase desc $
    verifyResult argList expected
  where
    desc = "Should parse --file-log-strip-control none as StripControlNone"
    argList = ["--file-log-strip-control", "none", "command"]
    expected = updateDefArgs #fileLogStripControl StripControlNone

parseFileLogStripControlSmart :: TestTree
parseFileLogStripControlSmart =
  testCase desc $
    verifyResult argList expected
  where
    desc = "Should parse --file-log-strip-control smart as StripControlSmart"
    argList = ["--file-log-strip-control", "smart", "command"]
    expected = updateDefArgs #fileLogStripControl StripControlSmart

fileLogSizeModeSpecs :: TestTree
fileLogSizeModeSpecs =
  testGroup
    "File log size mode parsing"
    [ parseFileLogSizeWarn,
      parseFileLogSizeDelete
    ]

parseFileLogSizeWarn :: TestTree
parseFileLogSizeWarn =
  testCase "Should parse --file-log-size-mode warn" $
    verifyResult argList expected
  where
    argList = ["--file-log-size-mode", "warn 10 gb", "command"]
    expected =
      updateDefArgs
        #fileLogSizeMode
        (FileSizeModeWarn $ MkBytes 10_000_000_000)

parseFileLogSizeDelete :: TestTree
parseFileLogSizeDelete =
  testCase "Should parse --file-log-size-mode delete" $
    verifyResult argList expected
  where
    argList = ["--file-log-size-mode", "delete 2.4Kilobytes", "command"]
    expected =
      updateDefArgs
        #fileLogSizeMode
        (FileSizeModeDelete $ MkBytes 2_400)

commandLoggingSpecs :: TestTree
commandLoggingSpecs =
  testGroup
    "CmdLogging arg parsing"
    [ parseShortCommandLogging,
      parseLongCommandLogging
    ]

parseShortCommandLogging :: TestTree
parseShortCommandLogging = testCase "Should parse -l as CmdLogging" $ do
  verifyResult argList expected
  where
    argList = ["-l", "command"]
    expected = updateDefArgs #cmdLogging True

parseLongCommandLogging :: TestTree
parseLongCommandLogging =
  testCase "Should parse --cmd-log as CmdLogging" $
    verifyResult argList expected
  where
    argList = ["--cmd-log", "command"]
    expected = updateDefArgs #cmdLogging True

commandDisplaySpecs :: TestTree
commandDisplaySpecs =
  testGroup
    "CmdDisplay arg parsing"
    [ parseShortShowKey,
      parseLongShowKey
    ]

parseShortShowKey :: TestTree
parseShortShowKey =
  testCase "Should parse -k as HideKey" $
    verifyResult argList expected
  where
    argList = ["-k", "command"]
    expected = updateDefArgs #cmdDisplay HideKey

parseLongShowKey :: TestTree
parseLongShowKey =
  testCase "Should parse --key-hide as ShowKey" $
    verifyResult argList expected
  where
    argList = ["--key-hide", "command"]
    expected = updateDefArgs #cmdDisplay HideKey

pollIntervalSpecs :: TestTree
pollIntervalSpecs =
  testGroup
    "CmdDisplay arg parsing"
    [ parseShortPollInterval,
      parseLongPollInterval
    ]

parseShortPollInterval :: TestTree
parseShortPollInterval =
  testCase "Should parse -p as poll-interval" $
    verifyResult argList expected
  where
    argList = ["-p", "100", "command"]
    expected = updateDefArgs #pollInterval 100

parseLongPollInterval :: TestTree
parseLongPollInterval =
  testCase "Should parse --poll-interval as poll-interval" $
    verifyResult argList expected
  where
    argList = ["--poll-interval", "1000", "command"]
    expected = updateDefArgs #pollInterval 1000

stripControlSpecs :: TestTree
stripControlSpecs =
  testGroup
    "Strip control arg parsing"
    [ parseShortStripControlAll,
      parseShortStripControlNone,
      parseShortStripControlSmart,
      parseLongStripControlSmart
    ]

parseShortStripControlAll :: TestTree
parseShortStripControlAll =
  testCase "Should parse -sall as StripControlAll" $
    verifyResult argList expected
  where
    argList = ["-sall", "command"]
    expected = updateDefArgs #cmdLogStripControl StripControlAll

parseShortStripControlNone :: TestTree
parseShortStripControlNone =
  testCase desc $
    verifyResult argList expected
  where
    desc = "Should parse -snone as StripControlNone"
    argList = ["-snone", "command"]
    expected = updateDefArgs #cmdLogStripControl StripControlNone

parseShortStripControlSmart :: TestTree
parseShortStripControlSmart =
  testCase desc $
    verifyResult argList expected
  where
    desc = "Should parse -ssmart as StripControlSmart"
    argList = ["-ssmart", "command"]
    expected = updateDefArgs #cmdLogStripControl StripControlSmart

parseLongStripControlSmart :: TestTree
parseLongStripControlSmart =
  testCase desc $
    verifyResult argList expected
  where
    desc = "Should parse --cmd-log-strip-control=smart as StripControlSmart"
    argList = ["--cmd-log-strip-control=smart", "command"]
    expected = updateDefArgs #cmdLogStripControl StripControlSmart

cmdNameTruncSpecs :: TestTree
cmdNameTruncSpecs =
  testGroup
    "Command name truncation arg parsing"
    [ parseShortCmdNameTrunc,
      parseLongCmdNameTrunc
    ]

parseShortCmdNameTrunc :: TestTree
parseShortCmdNameTrunc =
  testCase desc $
    verifyResult argList expected
  where
    desc = "Should parse -x as command name truncation"
    argList = ["-x", "15", "command"]
    expected = updateDefArgs #cmdNameTrunc 15

parseLongCmdNameTrunc :: TestTree
parseLongCmdNameTrunc =
  testCase
    "Should parse --cmd-name-trunc as command name truncation"
    $ verifyResult argList expected
  where
    argList = ["--cmd-name-trunc", "15", "command"]
    expected = updateDefArgs #cmdNameTrunc 15

cmdLineTruncSpecs :: TestTree
cmdLineTruncSpecs =
  testGroup
    "Command line truncation arg parsing"
    [ parseShortCmdLineTrunc,
      parseLongCmdLineTrunc,
      parseDetectCmdLineTrunc
    ]

parseShortCmdLineTrunc :: TestTree
parseShortCmdLineTrunc =
  testCase desc $
    verifyResult argList expected
  where
    desc = "Should parse -y as command line truncation"
    argList = ["-y", "15", "command"]
    expected = updateDefArgs #cmdLogLineTrunc (Undetected 15)

parseLongCmdLineTrunc :: TestTree
parseLongCmdLineTrunc =
  testCase
    "Should parse --cmd-log-line-trunc as command line truncation"
    $ verifyResult argList expected
  where
    argList = ["--cmd-log-line-trunc", "15", "command"]
    expected = updateDefArgs #cmdLogLineTrunc (Undetected 15)

parseDetectCmdLineTrunc :: TestTree
parseDetectCmdLineTrunc =
  testCase desc $
    verifyResult argList expected
  where
    desc = "Should parse --cmd-log-line-trunc detect as detect command line truncation"
    argList = ["--cmd-log-line-trunc", "detect", "command"]
    expected = updateDefArgs #cmdLogLineTrunc Detected

notifySystemSpecs :: TestTree
notifySystemSpecs =
  testGroup
    "Notify system parsing"
    [ parseNotifySystemDBus,
      parseNotifySystemNotifySend
    ]

parseNotifySystemDBus :: TestTree
parseNotifySystemDBus = testCase desc $ verifyResult argList expected
  where
    desc = "Should parse --notify-system dbus"
    argList = ["--notify-system", "dbus", "command"]
    expected = updateDefArgs #notifySystem (DBus ())

parseNotifySystemNotifySend :: TestTree
parseNotifySystemNotifySend = testCase desc $ verifyResult argList expected
  where
    desc = "Should parse --notify-system notify-send"
    argList = ["--notify-system", "notify-send", "command"]
    expected = updateDefArgs #notifySystem NotifySend

notifyActionSpecs :: TestTree
notifyActionSpecs =
  testGroup
    "Notify action parsing"
    [ parseNotifyActionNone,
      parseNotifyActionFinal,
      parseNotifyActionCommand
    ]

parseNotifyActionNone :: TestTree
parseNotifyActionNone = testCase desc $ verifyResult argList expected
  where
    desc = "Should parse --notify-action none"
    argList = ["--notify-action", "none", "command"]
    expected = updateDefArgs #notifyAction NotifyNone

parseNotifyActionFinal :: TestTree
parseNotifyActionFinal = testCase desc $ verifyResult argList expected
  where
    desc = "Should parse --notify-action final"
    argList = ["--notify-action", "final", "command"]
    expected = updateDefArgs #notifyAction NotifyFinal

parseNotifyActionCommand :: TestTree
parseNotifyActionCommand = testCase desc $ verifyResult argList expected
  where
    desc = "Should parse --notify-action command"
    argList = ["--notify-action", "command", "command"]
    expected = updateDefArgs #notifyAction NotifyCommand

notifyTimeoutSpecs :: TestTree
notifyTimeoutSpecs =
  testGroup
    "Notify timeout parsing"
    [ parseNotifyTimeoutSeconds,
      parseNotifyTimeoutNever
    ]

parseNotifyTimeoutSeconds :: TestTree
parseNotifyTimeoutSeconds = testCase desc $ verifyResult argList expected
  where
    desc = "Should parse --notify-timeout 5"
    argList = ["--notify-timeout", "5", "command"]
    expected = updateDefArgs #notifyTimeout (NotifyTimeoutSeconds 5)

parseNotifyTimeoutNever :: TestTree
parseNotifyTimeoutNever = testCase desc $ verifyResult argList expected
  where
    desc = "Should parse --notify-timeout never"
    argList = ["--notify-timeout", "never", "command"]
    expected = updateDefArgs #notifyTimeout NotifyTimeoutNever

commandSpecs :: TestTree
commandSpecs =
  testGroup
    "Command arg parsing"
    [ emptyCommandsFail,
      parseCommands
    ]

emptyCommandsFail :: TestTree
emptyCommandsFail =
  testCase "Empty commands fail" $
    verifyResult [] Nothing

parseCommands :: TestTree
parseCommands =
  testCase "Bare strings parsed as commands" $
    verifyResult argList expected
  where
    argList = ["one", "two", "three"]
    expected = ((_Just % #commands) .~ cmds) defArgs
    cmds = U.unsafeListToNESeq ["one", "two", "three"]

verifyResult :: List String -> Maybe Args -> Assertion
verifyResult argList expected = do
  let result = OptApp.execParserPure prefs Args.parserInfoArgs argList
  expected @=? OptApp.getParseResult result

prefs :: ParserPrefs
prefs = OptApp.prefs mempty

defCommand :: NESeq Text
defCommand = "command" :<|| []

defArgs :: Maybe Args
defArgs = Just $ Args.defaultArgs defCommand

updateDefArgs :: Lens' Args (Maybe a) -> a -> Maybe Args
updateDefArgs l x = ((_Just % l) ?~ x) defArgs
