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
    Truncation (..),
  )
import Shrun.Data.FilePathDefault (FilePathDefault (..))
import Shrun.Data.NonEmptySeq (NonEmptySeq)
import Shrun.Data.NonEmptySeq qualified as NESeq
import Shrun.Data.Timeout (Timeout (..))
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
              fileLogging = Nothing,
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
parseShortConfig = testCase "Should parse short config" $ do
  let argList = ["-c./path/config.toml", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { configPath = Just "./path/config.toml"
            }
  verifyResult argList expected

parseLongConfig :: TestTree
parseLongConfig = testCase "Should parse long config" $ do
  let argList = ["--config=./path/config.toml", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { configPath = Just "./path/config.toml"
            }
  verifyResult argList expected

parseNoConfig :: TestTree
parseNoConfig = testCase "Should parse no-config" $ do
  let argList = ["--no-config", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { noConfig = True
            }
  verifyResult argList expected

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
parseShortTimeout = testCase "Should parse short timeout" $ do
  let argList = ["-t7", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {timeout = toTO 7}
  verifyResult argList expected

parseLongTimeout :: TestTree
parseLongTimeout = testCase "Should parse long timeout" $ do
  let argList = ["--timeout=7", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {timeout = toTO 7}
  verifyResult argList expected

parseTimeString :: TestTree
parseTimeString = testCase "Should parse time string" $ do
  let argList = ["--timeout=2h4s", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {timeout = toTO 7204}
  verifyResult argList expected

parseLongTimeString :: TestTree
parseLongTimeString = testCase "Should parse full time string" $ do
  let argList = ["--timeout=1d2h3m4s", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {timeout = toTO 93784}
  verifyResult argList expected

parseTimeoutWordFail :: TestTree
parseTimeoutWordFail = testCase "Word should fail" $ do
  let argList = ["--timeout=cat", "command"]
      expected = Nothing
  verifyResult argList expected

parseNegativeTimeoutFail :: TestTree
parseNegativeTimeoutFail = testCase "Negative should fail" $ do
  let argList = ["--timeout=-7", "command"]
      expected = Nothing
  verifyResult argList expected

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
parseShortFileLogging = testCase "Should parse filepath with -f" $ do
  let argList = ["-flogfile", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { fileLogging = Just $ FPManual "logfile"
            }
  verifyResult argList expected

parseLongFileLogging :: TestTree
parseLongFileLogging = testCase "Should parse filepath with --file-log" $ do
  let argList = ["--file-log=logfile", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { fileLogging = Just $ FPManual "logfile"
            }
  verifyResult argList expected

parseLongDefaultFileLogging :: TestTree
parseLongDefaultFileLogging = testCase "Should parse default --file-log" $ do
  let argList = ["--file-log", "default", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { fileLogging = Just FPDefault
            }
  verifyResult argList expected

parseShortDefaultFileLogging :: TestTree
parseShortDefaultFileLogging = testCase "Should parse default -f" $ do
  let argList = ["-f", "default", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { fileLogging = Just FPDefault
            }
  verifyResult argList expected

parseShortEmptyFileLoggingFails :: TestTree
parseShortEmptyFileLoggingFails = testCase "Should parse empty -f as failure" $ do
  let argList = ["-f", "command"]
      expected = Nothing
  verifyResult argList expected

parseLongEmptyFileLoggingFails :: TestTree
parseLongEmptyFileLoggingFails = testCase "Should parse empty --file-log as failure" $ do
  let argList = ["--file-log=", "command"]
      expected = Nothing
  verifyResult argList expected

fileLogModeSpecs :: TestTree
fileLogModeSpecs =
  testGroup
    "File log mode"
    [ parseFileLogModeAppend,
      parseFileLogModeWrite
    ]

parseFileLogModeAppend :: TestTree
parseFileLogModeAppend = testCase
  "Should parse --file-log-mode append as FileModeAppend"
  $ do
    let argList = ["--file-log-mode", "append", "command"]
        expected =
          Just $
            (Args.defaultArgs defCommand)
              { fileLogMode = Just FileModeAppend
              }
    verifyResult argList expected

parseFileLogModeWrite :: TestTree
parseFileLogModeWrite = testCase
  "Should parse --file-log-mode write as FileModeWrite"
  $ do
    let argList = ["--file-log-mode", "write", "command"]
        expected =
          Just $
            (Args.defaultArgs defCommand)
              { fileLogMode = Just FileModeWrite
              }
    verifyResult argList expected

fileLogStripControlSpecs :: TestTree
fileLogStripControlSpecs =
  testGroup
    "Strip control arg parsing"
    [ parseFileLogStripControlAll,
      parseFileLogStripControlNone,
      parseFileLogStripControlSmart
    ]

parseFileLogStripControlAll :: TestTree
parseFileLogStripControlAll = testCase
  "Should parse --file-log-strip-control all as StripControlAll"
  $ do
    let argList = ["--file-log-strip-control", "all", "command"]
        expected =
          Just $
            (Args.defaultArgs defCommand)
              { fileLogStripControl = Just StripControlAll
              }
    verifyResult argList expected

parseFileLogStripControlNone :: TestTree
parseFileLogStripControlNone = testCase
  "Should parse --file-log-strip-control none as StripControlNone"
  $ do
    let argList = ["--file-log-strip-control", "none", "command"]
        expected =
          Just $
            (Args.defaultArgs defCommand)
              { fileLogStripControl = Just StripControlNone
              }
    verifyResult argList expected

parseFileLogStripControlSmart :: TestTree
parseFileLogStripControlSmart = testCase
  "Should parse --file-log-strip-control smart as StripControlSmart"
  $ do
    let argList = ["--file-log-strip-control", "smart", "command"]
        expected =
          Just $
            (Args.defaultArgs defCommand)
              { fileLogStripControl = Just StripControlSmart
              }
    verifyResult argList expected

fileLogSizeModeSpecs :: TestTree
fileLogSizeModeSpecs =
  testGroup
    "File log size mode parsing"
    [ parseFileLogSizeWarn,
      parseFileLogSizeDelete
    ]

parseFileLogSizeWarn :: TestTree
parseFileLogSizeWarn = testCase
  "Should parse --file-log-size-mode warn"
  $ do
    let argList = ["--file-log-size-mode", "warn 10 gb", "command"]
        expected =
          Just $
            (Args.defaultArgs defCommand)
              { fileLogSizeMode = Just $ FileSizeModeWarn $ MkBytes 10_000_000_000
              }
    verifyResult argList expected

parseFileLogSizeDelete :: TestTree
parseFileLogSizeDelete = testCase
  "Should parse --file-log-size-mode delete"
  $ do
    let argList = ["--file-log-size-mode", "delete 2.4Kilobytes", "command"]
        expected =
          Just $
            (Args.defaultArgs defCommand)
              { fileLogSizeMode = Just $ FileSizeModeDelete $ MkBytes 2_400
              }
    verifyResult argList expected

commandLoggingSpecs :: TestTree
commandLoggingSpecs =
  testGroup
    "CmdLogging arg parsing"
    [ parseShortCommandLogging,
      parseLongCommandLogging
    ]

parseShortCommandLogging :: TestTree
parseShortCommandLogging = testCase "Should parse -l as CmdLogging" $ do
  let argList = ["-l", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { cmdLogging = Just Enabled
            }
  verifyResult argList expected

parseLongCommandLogging :: TestTree
parseLongCommandLogging = testCase "Should parse --cmd-log as CmdLogging" $ do
  let argList = ["--cmd-log", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { cmdLogging = Just Enabled
            }
  verifyResult argList expected

commandDisplaySpecs :: TestTree
commandDisplaySpecs =
  testGroup
    "CmdDisplay arg parsing"
    [ parseShortShowKey,
      parseLongShowKey
    ]

parseShortShowKey :: TestTree
parseShortShowKey = testCase "Should parse -k as HideKey" $ do
  let argList = ["-k", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { cmdDisplay = Just HideKey
            }
  verifyResult argList expected

parseLongShowKey :: TestTree
parseLongShowKey = testCase "Should parse --key-hide as ShowKey" $ do
  let argList = ["--key-hide", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { cmdDisplay = Just HideKey
            }
  verifyResult argList expected

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
parseShortStripControlAll = testCase "Should parse -sall as StripControlAll" $ do
  let argList = ["-sall", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { stripControl = Just StripControlAll
            }
  verifyResult argList expected

parseShortStripControlNone :: TestTree
parseShortStripControlNone = testCase "Should parse -snone as StripControlNone" $ do
  let argList = ["-snone", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { stripControl = Just StripControlNone
            }
  verifyResult argList expected

parseShortStripControlSmart :: TestTree
parseShortStripControlSmart = testCase "Should parse -ssmart as StripControlSmart" $ do
  let argList = ["-ssmart", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { stripControl = Just StripControlSmart
            }
  verifyResult argList expected

parseLongStripControlSmart :: TestTree
parseLongStripControlSmart = testCase "Should parse --strip-control=smart as StripControlSmart" $ do
  let argList = ["--strip-control=smart", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { stripControl = Just StripControlSmart
            }
  verifyResult argList expected

cmdNameTruncSpecs :: TestTree
cmdNameTruncSpecs =
  testGroup
    "Command name truncation arg parsing"
    [ parseShortCmdNameTrunc,
      parseLongCmdNameTrunc
    ]

parseShortCmdNameTrunc :: TestTree
parseShortCmdNameTrunc = testCase "Should parse -x as command name truncation" $ do
  let argList = ["-x", "15", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { cmdNameTrunc = Just $ MkTruncation 15
            }
  verifyResult argList expected

parseLongCmdNameTrunc :: TestTree
parseLongCmdNameTrunc = testCase
  "Should parse --cmd-name-trunc as command name truncation"
  $ do
    let argList = ["--cmd-name-trunc", "15", "command"]
        expected =
          Just $
            (Args.defaultArgs defCommand)
              { cmdNameTrunc = Just $ MkTruncation 15
              }
    verifyResult argList expected

cmdLineTruncSpecs :: TestTree
cmdLineTruncSpecs =
  testGroup
    "Command line truncation arg parsing"
    [ parseShortCmdLineTrunc,
      parseLongCmdLineTrunc,
      parseDetectCmdLineTrunc
    ]

parseShortCmdLineTrunc :: TestTree
parseShortCmdLineTrunc = testCase "Should parse -y as command line truncation" $ do
  let argList = ["-y", "15", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { cmdLineTrunc = Just $ Undetected $ MkTruncation 15
            }
  verifyResult argList expected

parseLongCmdLineTrunc :: TestTree
parseLongCmdLineTrunc = testCase
  "Should parse --cmd-line-trunc as command line truncation"
  $ do
    let argList = ["--cmd-line-trunc", "15", "command"]
        expected =
          Just $
            (Args.defaultArgs defCommand)
              { cmdLineTrunc = Just $ Undetected $ MkTruncation 15
              }
    verifyResult argList expected

parseDetectCmdLineTrunc :: TestTree
parseDetectCmdLineTrunc = testCase
  "Should parse --cmd-line-trunc detect as detect command line truncation"
  $ do
    let argList = ["--cmd-line-trunc", "detect", "command"]
        expected =
          Just $
            (Args.defaultArgs defCommand)
              { cmdLineTrunc = Just Detected
              }
    verifyResult argList expected

globalLoggingSpecs :: TestTree
globalLoggingSpecs =
  testGroup
    "Global logging arg parsing"
    [ parseShortGlobalLogging,
      parseLongGlobalLogging
    ]

parseShortGlobalLogging :: TestTree
parseShortGlobalLogging = testCase "Should parse -d as no global logging" $ do
  let argList = ["-d", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { disableLogging = Just True
            }
  verifyResult argList expected

parseLongGlobalLogging :: TestTree
parseLongGlobalLogging = testCase "Should parse --disable-log as no global logging" $ do
  let argList = ["--disable-log", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { disableLogging = Just True
            }
  verifyResult argList expected

commandSpecs :: TestTree
commandSpecs =
  testGroup
    "Command arg parsing"
    [ emptyCommandsFail,
      parseCommands
    ]

emptyCommandsFail :: TestTree
emptyCommandsFail = testCase "Empty commands fail" $ do
  let argList = []
      expected = Nothing
  verifyResult argList expected

parseCommands :: TestTree
parseCommands = testCase
  "Bare strings parsed as commands"
  $ do
    let argList = ["one", "two", "three"]
        expected = Just $ (Args.defaultArgs defCommand) {commands = NESeq.unsafeFromList ["one", "two", "three"]}
    verifyResult argList expected

verifyResult :: List String -> Maybe Args -> Assertion
verifyResult argList expected = do
  let result = OptApp.execParserPure prefs Args.parserInfoArgs argList
  expected @=? OptApp.getParseResult result

prefs :: ParserPrefs
prefs = OptApp.prefs mempty

toTO :: Natural -> Maybe Timeout
toTO = Just . MkTimeout

defCommand :: NonEmptySeq Text
defCommand = NESeq.unsafeFromList ["command"]
