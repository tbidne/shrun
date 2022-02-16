-- | Specs for ShellRun.Parsing.Args.
module Specs.ShellRun.Parsing.Args (specs) where

import Data.String (String)
import Options.Applicative (ParserPrefs)
import Options.Applicative qualified as OptApp
import Refined.Unsafe qualified as R
import ShellRun.Data.Env (CommandDisplay (..), CommandLogging (..))
import ShellRun.Data.NonEmptySeq (NonEmptySeq)
import ShellRun.Data.NonEmptySeq qualified as NESeq
import ShellRun.Data.Timeout (Timeout (..))
import ShellRun.Parsing.Args (Args (..))
import ShellRun.Parsing.Args qualified as Args
import ShellRun.Prelude
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit (Assertion, (@=?))
import Test.Tasty.HUnit qualified as THU

-- | Entry point for ShellRun.Parsing.Args specs.
specs :: TestTree
specs =
  Tasty.testGroup
    "ShellRun.Parsing.Args"
    [ defaultSpec,
      legendSpecs,
      timeoutSpecs,
      fileLoggingSpecs,
      commandLoggingSpecs,
      commandDisplaySpecs,
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
          (Args.defaultArgs defCommand)
            { aLegend = Nothing,
              aTimeout = Nothing,
              aCommandLogging = Disabled,
              aCommandDisplay = ShowCommand
            }
  verifyResult argList expected

legendSpecs :: TestTree
legendSpecs =
  Tasty.testGroup
    "Legend arg parsing"
    [ parseShortLegend,
      parseLongLegend
    ]

parseShortLegend :: TestTree
parseShortLegend = THU.testCase "Should parse short legend" $ do
  let argList = ["-l./path/legend.txt", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {aLegend = Just "./path/legend.txt"}
  verifyResult argList expected

parseLongLegend :: TestTree
parseLongLegend = THU.testCase "Should parse long legend" $ do
  let argList = ["--legend=./path/legend.txt", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {aLegend = Just "./path/legend.txt"}
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
      expected = Just $ (Args.defaultArgs defCommand) {aTimeout = toTO 7}
  verifyResult argList expected

parseLongTimeout :: TestTree
parseLongTimeout = THU.testCase "Should parse long timeout" $ do
  let argList = ["--timeout=7", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {aTimeout = toTO 7}
  verifyResult argList expected

parseTimeString :: TestTree
parseTimeString = THU.testCase "Should parse time string" $ do
  let argList = ["--timeout=2h4s", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {aTimeout = toTO 7204}
  verifyResult argList expected

parseLongTimeString :: TestTree
parseLongTimeString = THU.testCase "Should parse full time string" $ do
  let argList = ["--timeout=1d2h3m4s", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {aTimeout = toTO 93784}
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
      parseLongFileLogging
    ]

parseShortFileLogging :: TestTree
parseShortFileLogging = THU.testCase "Should parse filepath with -f" $ do
  let argList = ["-flogfile", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {aFileLogging = Just "logfile"}
  verifyResult argList expected

parseLongFileLogging :: TestTree
parseLongFileLogging = THU.testCase "Should parse filepath with --file-logging" $ do
  let argList = ["--file-logging=logfile", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {aFileLogging = Just "logfile"}
  verifyResult argList expected

commandLoggingSpecs :: TestTree
commandLoggingSpecs =
  Tasty.testGroup
    "CommandLogging arg parsing"
    [ parseShortCommandLogging,
      parseLongCommandLogging
    ]

parseShortCommandLogging :: TestTree
parseShortCommandLogging = THU.testCase "Should parse -c as CommandLogging" $ do
  let argList = ["-c", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {aCommandLogging = Enabled}
  verifyResult argList expected

parseLongCommandLogging :: TestTree
parseLongCommandLogging = THU.testCase "Should parse --command-logging as CommandLogging" $ do
  let argList = ["--command-logging", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {aCommandLogging = Enabled}
  verifyResult argList expected

commandDisplaySpecs :: TestTree
commandDisplaySpecs =
  Tasty.testGroup
    "CommandDisplay arg parsing"
    [ parseShortShowKey,
      parseLongShowKey
    ]

parseShortShowKey :: TestTree
parseShortShowKey = THU.testCase "Should parse -k as ShowKey" $ do
  let argList = ["-k", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {aCommandDisplay = ShowKey}
  verifyResult argList expected

parseLongShowKey :: TestTree
parseLongShowKey = THU.testCase "Should parse --key-show as ShowKey" $ do
  let argList = ["--key-show", "command"]
      expected = Just $ (Args.defaultArgs defCommand) {aCommandDisplay = ShowKey}
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
        expected = Just $ (Args.defaultArgs defCommand) {aCommands = NESeq.unsafeFromList ["one", "two", "three"]}
    verifyResult argList expected

verifyResult :: List String -> Maybe Args -> Assertion
verifyResult argList expected = do
  let result = OptApp.execParserPure prefs Args.parserInfoArgs argList
  expected @=? OptApp.getParseResult result

prefs :: ParserPrefs
prefs = OptApp.prefs mempty

toTO :: Int -> Maybe Timeout
toTO n = Just $ MkTimeout $ R.unsafeRefine n

defCommand :: NonEmptySeq Text
defCommand = NESeq.unsafeFromList ["command"]
