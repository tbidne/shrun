-- | Specs for ShellRun.Args.
module Unit.Specs.ShellRun.Args (specs) where

import Data.String (String)
import Options.Applicative (ParserPrefs)
import Options.Applicative qualified as OptApp
import ShellRun.Args (ALineTruncation (..), Args (..))
import ShellRun.Args qualified as Args
import ShellRun.Data.FilePathDefault (FilePathDefault (..))
import ShellRun.Data.InfNum (PosInfNum (..))
import ShellRun.Data.NonEmptySeq (NonEmptySeq)
import ShellRun.Data.NonEmptySeq qualified as NESeq
import ShellRun.Data.Timeout (Timeout (..))
import ShellRun.Env.Types
  ( CmdDisplay (..),
    CmdLogging (..),
    StripControl (..),
    Truncation (..),
  )
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as THU
import Unit.Prelude

-- | Entry point for ShellRun.Args specs.
specs :: TestTree
specs =
  Tasty.testGroup
    "ShellRun.Args"
    [ defaultSpec,
      legendSpecs,
      timeoutSpecs,
      fileLoggingSpecs,
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
  Tasty.testGroup
    "Default parsing"
    [parseDefaultArgs]

parseDefaultArgs :: TestTree
parseDefaultArgs = THU.testCase "Should parse default args" $ do
  let argList = ["command"]
      expected =
        Just $
          MkArgs
            { legend = FPDefault,
              timeout = MkTimeout PPosInf,
              cmdLogging = Disabled,
              cmdDisplay = ShowCmd,
              stripControl = StripControlAll,
              cmdNameTrunc = MkTruncation PPosInf,
              cmdLineTrunc = Undetected (MkTruncation PPosInf),
              fileLogging = FPNone,
              globalLogging = True,
              commands = NESeq.singleton "command"
            }
  verifyResult argList expected

legendSpecs :: TestTree
legendSpecs =
  Tasty.testGroup
    "Legend arg parsing"
    [ parseShortLegend,
      parseLongLegend,
      parseDefaultLegend
    ]

parseShortLegend :: TestTree
parseShortLegend = THU.testCase "Should parse short legend" $ do
  let argList = ["-l./path/shell-run.legend", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {legend = FPManual "./path/shell-run.legend"}
  verifyResult argList expected

parseLongLegend :: TestTree
parseLongLegend = THU.testCase "Should parse long legend" $ do
  let argList = ["--legend=./path/shell-run.legend", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {legend = FPManual "./path/shell-run.legend"}
  verifyResult argList expected

parseDefaultLegend :: TestTree
parseDefaultLegend = THU.testCase "Should parse default legend" $ do
  let argList = ["--legend", "default", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {legend = FPDefault}
  verifyResult argList expected

timeoutSpecs :: TestTree
timeoutSpecs =
  Tasty.testGroup
    "Timeout arg parsing"
    [ parseShortTimeout,
      parseLongTimeout,
      parseTimeString,
      parseLongTimeString,
      parseTimeoutWordFail,
      parseNegativeTimeoutFail
    ]

parseShortTimeout :: TestTree
parseShortTimeout = THU.testCase "Should parse short timeout" $ do
  let argList = ["-t7", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {timeout = toTO 7}
  verifyResult argList expected

parseLongTimeout :: TestTree
parseLongTimeout = THU.testCase "Should parse long timeout" $ do
  let argList = ["--timeout=7", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {timeout = toTO 7}
  verifyResult argList expected

parseTimeString :: TestTree
parseTimeString = THU.testCase "Should parse time string" $ do
  let argList = ["--timeout=2h4s", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {timeout = toTO 7204}
  verifyResult argList expected

parseLongTimeString :: TestTree
parseLongTimeString = THU.testCase "Should parse full time string" $ do
  let argList = ["--timeout=1d2h3m4s", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {timeout = toTO 93784}
  verifyResult argList expected

parseTimeoutWordFail :: TestTree
parseTimeoutWordFail = THU.testCase "Word should fail" $ do
  let argList = ["--timeout=cat", "command"]
      expected = Nothing
  verifyResult argList expected

parseNegativeTimeoutFail :: TestTree
parseNegativeTimeoutFail = THU.testCase "Negative should fail" $ do
  let argList = ["--timeout=-7", "command"]
      expected = Nothing
  verifyResult argList expected

fileLoggingSpecs :: TestTree
fileLoggingSpecs =
  Tasty.testGroup
    "FileLogging arg parsing"
    [ parseShortFileLogging,
      parseLongFileLogging,
      parseDefaultFileLogging,
      parseDFileLogging
    ]

parseShortFileLogging :: TestTree
parseShortFileLogging = THU.testCase "Should parse filepath with -f" $ do
  let argList = ["-flogfile", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {fileLogging = FPManual "logfile"}
  verifyResult argList expected

parseLongFileLogging :: TestTree
parseLongFileLogging = THU.testCase "Should parse filepath with --file-log" $ do
  let argList = ["--file-log=logfile", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {fileLogging = FPManual "logfile"}
  verifyResult argList expected

parseDefaultFileLogging :: TestTree
parseDefaultFileLogging = THU.testCase "Should parse default --file-log" $ do
  let argList = ["--file-log", "default", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {fileLogging = FPDefault}
  verifyResult argList expected

parseDFileLogging :: TestTree
parseDFileLogging = THU.testCase "Should parse default -fd" $ do
  let argList = ["-f", "d", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {fileLogging = FPDefault}
  verifyResult argList expected

commandLoggingSpecs :: TestTree
commandLoggingSpecs =
  Tasty.testGroup
    "CmdLogging arg parsing"
    [ parseShortCommandLogging,
      parseLongCommandLogging
    ]

parseShortCommandLogging :: TestTree
parseShortCommandLogging = THU.testCase "Should parse -c as CmdLogging" $ do
  let argList = ["-c", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {cmdLogging = Enabled}
  verifyResult argList expected

parseLongCommandLogging :: TestTree
parseLongCommandLogging = THU.testCase "Should parse --cmd-log as CmdLogging" $ do
  let argList = ["--cmd-log", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {cmdLogging = Enabled}
  verifyResult argList expected

commandDisplaySpecs :: TestTree
commandDisplaySpecs =
  Tasty.testGroup
    "CmdDisplay arg parsing"
    [ parseShortShowKey,
      parseLongShowKey
    ]

parseShortShowKey :: TestTree
parseShortShowKey = THU.testCase "Should parse -k as ShowKey" $ do
  let argList = ["-k", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {cmdDisplay = ShowKey}
  verifyResult argList expected

parseLongShowKey :: TestTree
parseLongShowKey = THU.testCase "Should parse --key-show as ShowKey" $ do
  let argList = ["--key-show", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {cmdDisplay = ShowKey}
  verifyResult argList expected

stripControlSpecs :: TestTree
stripControlSpecs =
  Tasty.testGroup
    "Strip control arg parsing"
    [ parseShortStripControlAll,
      parseShortStripControlNone,
      parseShortStripControlSmart,
      parseLongStripControlSmart
    ]

parseShortStripControlAll :: TestTree
parseShortStripControlAll = THU.testCase "Should parse -sall as StripControlAll" $ do
  let argList = ["-sall", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { stripControl = StripControlAll
            }
  verifyResult argList expected

parseShortStripControlNone :: TestTree
parseShortStripControlNone = THU.testCase "Should parse -snone as StripControlNone" $ do
  let argList = ["-snone", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { stripControl = StripControlNone
            }
  verifyResult argList expected

parseShortStripControlSmart :: TestTree
parseShortStripControlSmart = THU.testCase "Should parse -ssmart as StripControlSmart" $ do
  let argList = ["-ssmart", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { stripControl = StripControlSmart
            }
  verifyResult argList expected

parseLongStripControlSmart :: TestTree
parseLongStripControlSmart = THU.testCase "Should parse --strip-control=smart as StripControlSmart" $ do
  let argList = ["--strip-control=smart", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { stripControl = StripControlSmart
            }
  verifyResult argList expected

cmdNameTruncSpecs :: TestTree
cmdNameTruncSpecs =
  Tasty.testGroup
    "Command name truncation arg parsing"
    [ parseShortCmdNameTrunc,
      parseLongCmdNameTrunc
    ]

parseShortCmdNameTrunc :: TestTree
parseShortCmdNameTrunc = THU.testCase "Should parse -x as command name truncation" $ do
  let argList = ["-x", "15", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { cmdNameTrunc = MkTruncation (PFin 15)
            }
  verifyResult argList expected

parseLongCmdNameTrunc :: TestTree
parseLongCmdNameTrunc = THU.testCase
  "Should parse --cmd-name-trunc as command name truncation"
  $ do
    let argList = ["--cmd-name-trunc", "15", "command"]
        expected =
          Just $
            (Args.defaultArgs defCommand)
              { cmdNameTrunc = MkTruncation (PFin 15)
              }
    verifyResult argList expected

cmdLineTruncSpecs :: TestTree
cmdLineTruncSpecs =
  Tasty.testGroup
    "Command line truncation arg parsing"
    [ parseShortCmdLineTrunc,
      parseLongCmdLineTrunc,
      parseDetectCmdLineTrunc,
      parseDCmdLineTrunc
    ]

parseShortCmdLineTrunc :: TestTree
parseShortCmdLineTrunc = THU.testCase "Should parse -y as command line truncation" $ do
  let argList = ["-y", "15", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { cmdLineTrunc = Undetected $ MkTruncation (PFin 15)
            }
  verifyResult argList expected

parseLongCmdLineTrunc :: TestTree
parseLongCmdLineTrunc = THU.testCase
  "Should parse --cmd-line-trunc as command line truncation"
  $ do
    let argList = ["--cmd-line-trunc", "15", "command"]
        expected =
          Just $
            (Args.defaultArgs defCommand)
              { cmdLineTrunc = Undetected $ MkTruncation (PFin 15)
              }
    verifyResult argList expected

parseDetectCmdLineTrunc :: TestTree
parseDetectCmdLineTrunc = THU.testCase
  "Should parse --cmd-line-trunc detect as detect command line truncation"
  $ do
    let argList = ["--cmd-line-trunc", "detect", "command"]
        expected =
          Just $
            (Args.defaultArgs defCommand)
              { cmdLineTrunc = Detected
              }
    verifyResult argList expected

parseDCmdLineTrunc :: TestTree
parseDCmdLineTrunc = THU.testCase
  "Should parse -yd as detect command line truncation"
  $ do
    let argList = ["-y", "d", "command"]
        expected =
          Just $
            (Args.defaultArgs defCommand)
              { cmdLineTrunc = Detected
              }
    verifyResult argList expected

globalLoggingSpecs :: TestTree
globalLoggingSpecs =
  Tasty.testGroup
    "Global logging arg parsing"
    [ parseShortGlobalLogging,
      parseLongGlobalLogging
    ]

parseShortGlobalLogging :: TestTree
parseShortGlobalLogging = THU.testCase "Should parse -d as no global logging" $ do
  let argList = ["-d", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { globalLogging = False
            }
  verifyResult argList expected

parseLongGlobalLogging :: TestTree
parseLongGlobalLogging = THU.testCase "Should parse --disable-log as no global logging" $ do
  let argList = ["--disable-log", "command"]
      expected =
        Just $
          (Args.defaultArgs defCommand)
            { globalLogging = False
            }
  verifyResult argList expected

commandSpecs :: TestTree
commandSpecs =
  Tasty.testGroup
    "Command arg parsing"
    [ emptyCommandsFail,
      parseCommands
    ]

emptyCommandsFail :: TestTree
emptyCommandsFail = THU.testCase "Empty commands fail" $ do
  let argList = []
      expected = Nothing
  verifyResult argList expected

parseCommands :: TestTree
parseCommands = THU.testCase
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

toTO :: Natural -> Timeout
toTO n = MkTimeout $ PFin n

defCommand :: NonEmptySeq Text
defCommand = NESeq.unsafeFromList ["command"]
