-- | Functional test for a successful run that displays the key rather
-- than the command.
module Functional.SuccessShowKey (spec) where

import Functional.Prelude
import Functional.TestArgs (TestArgs (..))
import Functional.Utils qualified as U
import Test.ShellRun.Verifier (ExpectedText (..), ResultText (..), UnexpectedText (..))
import Test.ShellRun.Verifier qualified as V
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as THU

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
    MkTestArgs {legendPath} <- args
    withShowKey legendPath True

noShowKey :: IO TestArgs -> TestTree
noShowKey args =
  THU.testCase "Should show command rather than key" $ do
    MkTestArgs {legendPath} <- args
    withShowKey legendPath False

withShowKey :: FilePath -> Bool -> Assertion
withShowKey legendPath addShowKey = do
  let legendArg = "--legend=" <> legendPath
      argList = [legendArg, showKeyArg] <> commands

  results <- fmap MkResultText <$> (readIORef =<< U.runAndGetLogs argList)

  V.verifyExpectedUnexpected results expected unexpected
  where
    commands = ["both"]
    (showKeyArg, expected, unexpected) =
      if addShowKey
        then ("", showKeyExpected, showKeyUnexpected)
        else ("--key-hide", noShowKeyExpected, noShowKeyUnexpected)

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
