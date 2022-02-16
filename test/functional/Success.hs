-- | Functional test for a successful run.
module Success (spec) where

import Data.Text qualified as T
import ShellRun qualified as SR
import ShellRun.Env qualified as Env
import ShellRun.Prelude
import System.Environment qualified as SysEnv
import System.IO.Silently qualified as Shh
import Test.Tasty (TestTree)
import Test.Tasty.HUnit qualified as THU
import TestArgs (TestArgs (..))
import Utils qualified as U
import Verify (ExpectedText (..), ResultText (..), UnexpectedText (..))
import Verify qualified as V

-- | Spec that should run commands successfully.
spec :: IO TestArgs -> TestTree
spec args =
  THU.testCase "Should run commands successfully" $ do
    MkTestArgs {tLegendPath} <- args
    let legendArg = "--legend=" <> tLegendPath
        argList = [legendArg, timeout] <> commands

    env <- SysEnv.withArgs argList Env.runParser
    let action = runReaderT (SR.runShellT SR.runShell) env
    result <- Shh.capture_ action

    let results = MkResultText <$> T.lines (T.pack result)
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
