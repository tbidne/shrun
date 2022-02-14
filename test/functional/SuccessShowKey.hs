-- | Functional test for a successful run that displays the key rather
-- than the command.
module SuccessShowKey (spec) where

import Data.Text qualified as T
import ShellRun qualified as SR
import ShellRun.Parsing.Env qualified as Env
import ShellRun.Prelude
import System.Environment qualified as SysEnv
import System.IO.Silently qualified as Shh
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit (Assertion)
import Test.Tasty.HUnit qualified as THU
import TestArgs (TestArgs (..))
import Utils qualified as U
import Verify (ExpectedText (..), ResultText (..), UnexpectedText (..))
import Verify qualified as V

-- | Spec that should run commands displaying the key in the logs.
spec :: IO TestArgs -> TestTree
spec args = do
  Tasty.testGroup
    "Show Key tests"
    [ showKey args,
      noShowKey args
    ]

showKey :: IO TestArgs -> TestTree
showKey args =
  THU.testCase "Should show key rather than command" $ do
    MkTestArgs {tLegendPath} <- args
    withShowKey tLegendPath True

noShowKey :: IO TestArgs -> TestTree
noShowKey args =
  THU.testCase "Should show command rather than key" $ do
    MkTestArgs {tLegendPath} <- args
    withShowKey tLegendPath False

withShowKey :: FilePath -> Bool -> Assertion
withShowKey legendPath addShowKey = do
  let legendArg = "--legend=" <> legendPath
      argList = [legendArg, showKeyArg] <> commands

  env <- SysEnv.withArgs argList Env.runParser
  let action = runReaderT (SR.runShellT SR.runShell) env
  result <- Shh.capture_ action

  let results = MkResultText <$> T.lines (T.pack result)
  V.verifyExpectedUnexpected results expected unexpected
  where
    commands = ["both"]
    (showKeyArg, expected, unexpected) =
      if addShowKey
        then ("--show-key", showKeyExpected, showKeyUnexpected)
        else ("", noShowKeyExpected, noShowKeyUnexpected)

showKeyExpected :: List ExpectedText
showKeyExpected =
  MkExpectedText
    <$> [ U.infoSuccessPrefix "one",
          U.infoSuccessPrefix "long"
        ]

showKeyUnexpected :: List UnexpectedText
showKeyUnexpected =
  MkUnexpectedText
    <$> [ U.infoSuccessPrefix "sleep 1 && echo 1",
          U.infoSuccessPrefix "sleep 2 && echo long"
        ]

noShowKeyExpected :: List ExpectedText
noShowKeyExpected =
  MkExpectedText
    <$> [ U.infoSuccessPrefix "sleep 1 && echo 1",
          U.infoSuccessPrefix "sleep 2 && echo long"
        ]

noShowKeyUnexpected :: List UnexpectedText
noShowKeyUnexpected =
  MkUnexpectedText
    <$> [ U.infoSuccessPrefix "one",
          U.infoSuccessPrefix "long"
        ]
