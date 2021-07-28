-- | Specs for ShellRun.Parsing.Args.
module Specs.ShellRun.Parsing.Args (specs) where

import Data.String (String)
import Options.Applicative (ParserPrefs)
import Options.Applicative qualified as OptApp
import ShellRun.Data.Env (CommandDisplay (..), CommandLogging (..))
import ShellRun.Math qualified as Math
import ShellRun.Parsing.Args (Args (..))
import ShellRun.Parsing.Args qualified as Args
import ShellRun.Prelude
import Test.Hspec (Expectation, SpecWith, shouldBe)
import Test.Hspec qualified as Hspec
import Test.Tasty (TestTree)
import Test.Tasty.Hspec qualified as TH

-- | Entry point for ShellRun.Parsing.Args specs.
specs :: IO [TestTree]
specs = TH.testSpecs $ do
  Hspec.describe "ShellRun.Parsing.Args" $ do
    defaultSpec
    legendSpecs
    timeoutSpecs
    commandLoggingSpecs
    commandDisplaySpecs
    commandSpecs

defaultSpec :: SpecWith ()
defaultSpec = Hspec.describe "Default parsing" $ do
  Hspec.it "Should parse default args" $ do
    let argList = ["command"]
        expected =
          Just $
            mempty
              { aLegend = Nothing,
                aTimeout = Nothing,
                aCommandLogging = Disabled,
                aCommandDisplay = ShowCommand,
                aCommands = ["command"]
              }
    verifyResult argList expected

legendSpecs :: SpecWith ()
legendSpecs = Hspec.describe "Legend arg parsing" $ do
  Hspec.it "Should parse short legend" $ do
    let argList = ["-l./path/legend.txt", "command"]
        expected = Just $ mempty {aLegend = Just "./path/legend.txt", aCommands = ["command"]}
    verifyResult argList expected
  Hspec.it "Should parse long legend" $ do
    let argList = ["--legend=./path/legend.txt", "command"]
        expected = Just $ mempty {aLegend = Just "./path/legend.txt", aCommands = ["command"]}
    verifyResult argList expected

timeoutSpecs :: SpecWith ()
timeoutSpecs = Hspec.describe "Timeout arg parsing" $ do
  Hspec.it "Should parse short legend seconds" $ do
    let argList = ["-t7", "command"]
        expected = Just $ mempty {aTimeout = Math.mkNonNegative 7, aCommands = ["command"]}
    verifyResult argList expected
  Hspec.it "Should parse long legend seconds" $ do
    let argList = ["--timeout=7", "command"]
        expected = Just $ mempty {aTimeout = Math.mkNonNegative 7, aCommands = ["command"]}
    verifyResult argList expected
  Hspec.it "Should parse time string" $ do
    let argList = ["--timeout=2h4s", "command"]
        expected = Just $ mempty {aTimeout = Math.mkNonNegative 7204, aCommands = ["command"]}
    verifyResult argList expected
  Hspec.it "Should parse full time string" $ do
    let argList = ["--timeout=1d2h3m4s", "command"]
        expected = Just $ mempty {aTimeout = Math.mkNonNegative 93784, aCommands = ["command"]}
    verifyResult argList expected
  Hspec.it "Word should fail" $ do
    let argList = ["--timeout=cat", "command"]
        expected = Nothing
    verifyResult argList expected
  Hspec.it "Negative should fail" $ do
    let argList = ["--timeout=-7", "command"]
        expected = Nothing
    verifyResult argList expected

commandLoggingSpecs :: SpecWith ()
commandLoggingSpecs = Hspec.describe "CommandLogging arg parsing" $ do
  Hspec.it "Should parse -c as ShowKey" $ do
    let argList = ["-c", "command"]
        expected = Just $ mempty {aCommandLogging = Enabled, aCommands = ["command"]}
    verifyResult argList expected
  Hspec.it "Should parse --show-key as ShowKey" $ do
    let argList = ["--command-logging", "command"]
        expected = Just $ mempty {aCommandLogging = Enabled, aCommands = ["command"]}
    verifyResult argList expected

commandDisplaySpecs :: SpecWith ()
commandDisplaySpecs = Hspec.describe "CommandDisplay arg parsing" $ do
  Hspec.it "Should parse -k as ShowKey" $ do
    let argList = ["-k", "command"]
        expected = Just $ mempty {aCommandDisplay = ShowKey, aCommands = ["command"]}
    verifyResult argList expected
  Hspec.it "Should parse --show-key as ShowKey" $ do
    let argList = ["--show-key", "command"]
        expected = Just $ mempty {aCommandDisplay = ShowKey, aCommands = ["command"]}
    verifyResult argList expected

commandSpecs :: SpecWith ()
commandSpecs = Hspec.describe "CommandDisplay arg parsing" $ do
  Hspec.it "Empty commands fail" $ do
    let argList = []
        expected = Nothing
    verifyResult argList expected
  Hspec.it "Bare strings parsed as commands" $ do
    let argList = ["one", "two", "three"]
        expected = Just $ mempty {aCommands = ["one", "two", "three"]}
    verifyResult argList expected

verifyResult :: [String] -> Maybe Args -> Expectation
verifyResult argList expected = do
  let result = OptApp.execParserPure prefs Args.parserInfoArgs argList
  OptApp.getParseResult result `shouldBe` expected

prefs :: ParserPrefs
prefs = OptApp.prefs mempty
