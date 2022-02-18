-- | Functional test for a successful run with native logging.
module Truncation (spec) where

import Data.Text qualified as T
import ShellRun qualified as SR
import ShellRun.Env qualified as Env
import ShellRun.Prelude
import System.Environment qualified as SysEnv
import System.IO.Silently qualified as Shh
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as THU
import Utils qualified as U
import Verify (ExpectedText (..), ResultText (..))
import Verify qualified as V

-- | Spec that should run commands successfully and print stdout.
spec :: TestTree
spec =
  Tasty.testGroup
    "Truncation tests"
    [ THU.testCase "Should truncate command name" $ do
        env <- SysEnv.withArgs argList Env.runParser
        let action = runReaderT (SR.runShellT SR.runShell) env
        result <- Shh.capture_ action

        let results = MkResultText <$> T.lines (T.pack result)
        V.verifyExpected results expected
    ]
  where
    -- `sleep 1` is because commands that run too quickly will not have
    -- output logged
    commands = ["echo \"a long command\" && sleep 1"]
    argList =
      [ "-x 10"
      ]
        <> commands

expected :: List ExpectedText
expected =
  MkExpectedText
    <$> [ U.infoSuccessPrefix "echo \"a..."
        ]
