-- | Functional test for a successful run.
module Functional.Success (spec) where

import Functional.Prelude
import Functional.TestArgs (TestArgs (..))
import Functional.Utils qualified as U
import Test.ShellRun.Verifier (ExpectedText (..), ResultText (..), UnexpectedText (..))
import Test.ShellRun.Verifier qualified as V
import Test.Tasty.HUnit qualified as THU

-- | Spec that should run commands successfully.
spec :: IO TestArgs -> TestTree
spec args =
  THU.testCase "Should run commands successfully" $ do
    MkTestArgs {legendPath} <- args
    let legendArg = "--legend=" <> legendPath
        argList = [legendArg, timeout] <> commands

    results <- fmap MkResultText <$> (readIORef =<< U.runAndGetLogs argList)

    V.verifyExpectedUnexpected results allExpected allUnexpected
  where
    commands = ["bad", "both", "echo hi"]
    timeout = "--timeout=5"

allExpected :: List ExpectedText
allExpected =
  MkExpectedText
    <$> [ cmdEchoHi,
          cmdEcho1,
          cmdEchoLong,
          cmdBad,
          U.totalTime
        ]

allUnexpected :: List UnexpectedText
allUnexpected =
  MkUnexpectedText
    <$> [ cmdEchoHiStdout
        ]

cmdBad :: Text
cmdBad = U.errPrefix "some nonsense"

cmdEchoHi :: Text
cmdEchoHi = U.infoSuccessPrefix "echo hi"

cmdEcho1 :: Text
cmdEcho1 = U.infoSuccessPrefix "sleep 1 && echo 1"

cmdEchoLong :: Text
cmdEchoLong = U.infoSuccessPrefix "sleep 2 && echo long"

cmdEchoHiStdout :: Text
cmdEchoHiStdout = "echo hi: hi"
