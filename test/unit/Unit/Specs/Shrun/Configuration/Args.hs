-- | Specs for Shrun.Args.
module Unit.Specs.Shrun.Configuration.Args (specs) where

import Data.Bytes (Bytes (..))
import Options.Applicative (ParserPrefs)
import Options.Applicative qualified as OptApp
import Shrun.Configuration.Args (Args (..), FileMode (..), FileSizeMode (..))
import Shrun.Configuration.Args qualified as Args
import Shrun.Configuration.Env.Types
  ( CmdDisplay (..),
    CmdLogging (..),
    LineTruncation (..),
    StripControl (..),
  )
import Shrun.Data.FilePathDefault (FilePathDefault (..))
import Shrun.Data.NonEmptySeq (NonEmptySeq)
import Shrun.Data.NonEmptySeq qualified as NESeq
import Unit.Prelude

-- | Entry point for Shrun.Args specs.
specs :: TestTree
specs =
  testGroup
    "Shrun.Args"
    [ defaultSpec,
      configSpecs,
      timeoutSpecs,
      fileLoggingSpecs,
      fileLogModeSpecs,
      fileLogStripControlSpecs,
      fileLogSizeModeSpecs,
      commandLoggingSpecs,
      commandDisplaySpecs,
      stripControlSpecs,
      cmdNameTruncSpecs,
      cmdLineTruncSpecs,
      globalLoggingSpecs,
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
              cmdLogging = Nothing,
              cmdDisplay = Nothing,
              stripControl = Nothing,
              cmdNameTrunc = Nothing,
              cmdLineTrunc = Nothing,
              fileLog = Nothing,
              fileLogMode = Nothing,
              fileLogStripControl = Nothing,
              fileLogSizeMode = Nothing,
              disableLogging = Nothing,
              commands = NESeq.singleton "command"
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
    expected = updateDefArgs #fileLog (FPManual "logfile")

parseLongFileLogging :: TestTree
parseLongFileLogging =
  testCase "Should parse filepath with --file-log" $
    verifyResult argList expected
  where
    argList = ["--file-log=logfile", "command"]
    expected = updateDefArgs #fileLog (FPManual "logfile")

parseLongDefaultFileLogging :: TestTree
parseLongDefaultFileLogging =
  testCase "Should parse default --file-log" $
    verifyResult argList expected
  where
    argList = ["--file-log", "default", "command"]
    expected = updateDefArgs #fileLog FPDefault

parseShortDefaultFileLogging :: TestTree
parseShortDefaultFileLogging =
  testCase "Should parse default -f" $
    verifyResult argList expected
  where
    argList = ["-f", "default", "command"]
    expected = updateDefArgs #fileLog FPDefault

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
    expected = updateDefArgs #cmdLogging Enabled

parseLongCommandLogging :: TestTree
parseLongCommandLogging =
  testCase "Should parse --cmd-log as CmdLogging" $
    verifyResult argList expected
  where
    argList = ["--cmd-log", "command"]
    expected = updateDefArgs #cmdLogging Enabled

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
    expected = updateDefArgs #stripControl StripControlAll

parseShortStripControlNone :: TestTree
parseShortStripControlNone =
  testCase desc $
    verifyResult argList expected
  where
    desc = "Should parse -snone as StripControlNone"
    argList = ["-snone", "command"]
    expected = updateDefArgs #stripControl StripControlNone

parseShortStripControlSmart :: TestTree
parseShortStripControlSmart =
  testCase desc $
    verifyResult argList expected
  where
    desc = "Should parse -ssmart as StripControlSmart"
    argList = ["-ssmart", "command"]
    expected = updateDefArgs #stripControl StripControlSmart

parseLongStripControlSmart :: TestTree
parseLongStripControlSmart =
  testCase desc $
    verifyResult argList expected
  where
    desc = "Should parse --strip-control=smart as StripControlSmart"
    argList = ["--strip-control=smart", "command"]
    expected = updateDefArgs #stripControl StripControlSmart

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
    expected = updateDefArgs #cmdLineTrunc (Undetected 15)

parseLongCmdLineTrunc :: TestTree
parseLongCmdLineTrunc =
  testCase
    "Should parse --cmd-line-trunc as command line truncation"
    $ verifyResult argList expected
  where
    argList = ["--cmd-line-trunc", "15", "command"]
    expected = updateDefArgs #cmdLineTrunc (Undetected 15)

parseDetectCmdLineTrunc :: TestTree
parseDetectCmdLineTrunc =
  testCase desc $
    verifyResult argList expected
  where
    desc = "Should parse --cmd-line-trunc detect as detect command line truncation"
    argList = ["--cmd-line-trunc", "detect", "command"]
    expected = updateDefArgs #cmdLineTrunc Detected

globalLoggingSpecs :: TestTree
globalLoggingSpecs =
  testGroup
    "Global logging arg parsing"
    [ parseShortGlobalLogging,
      parseLongGlobalLogging
    ]

parseShortGlobalLogging :: TestTree
parseShortGlobalLogging =
  testCase "Should parse -d as no global logging" $
    verifyResult argList expected
  where
    argList = ["-d", "command"]
    expected = updateDefArgs #disableLogging True

parseLongGlobalLogging :: TestTree
parseLongGlobalLogging =
  testCase desc $
    verifyResult argList expected
  where
    desc = "Should parse --disable-log as no global logging"
    argList = ["--disable-log", "command"]
    expected = updateDefArgs #disableLogging True

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
    cmds = NESeq.unsafeFromList ["one", "two", "three"]

verifyResult :: List String -> Maybe Args -> Assertion
verifyResult argList expected = do
  let result = OptApp.execParserPure prefs Args.parserInfoArgs argList
  expected @=? OptApp.getParseResult result

prefs :: ParserPrefs
prefs = OptApp.prefs mempty

defCommand :: NonEmptySeq Text
defCommand = NESeq.unsafeFromList ["command"]

defArgs :: Maybe Args
defArgs = Just $ Args.defaultArgs defCommand

updateDefArgs :: Lens' Args (Maybe a) -> a -> Maybe Args
updateDefArgs l x = ((_Just % l) ?~ x) defArgs
