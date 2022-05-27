-- | Functional test for a run that should timeout.
module Functional.Timeout (spec) where

import Data.Text qualified as T
import Functional.Prelude
import Functional.Utils qualified as U
import ShellRun qualified as SR
import ShellRun.Env qualified as Env
import System.Environment qualified as SysEnv
import System.IO.Silently qualified as Shh
import Test.ShellRun.Verifier (ExpectedText (..), ResultText (..))
import Test.ShellRun.Verifier qualified as V
import Test.Tasty.HUnit qualified as THU

-- | Spec that should timeout.
spec :: TestTree
spec = THU.testCase "Should time out" $ do
  env <- SysEnv.withArgs argList Env.runParser
  let action = SR.runShellT SR.runShell env
  result <- Shh.capture_ action
  let results = MkResultText <$> T.lines (T.pack result)
  V.verifyExpected results allExpected
  where
    argList = timeout : commands
    commands = ["sleep 10"]
    timeout = "--timeout=5"

allExpected :: List ExpectedText
allExpected =
  MkExpectedText
    <$> [ U.cancelled,
          U.totalTime
        ]
