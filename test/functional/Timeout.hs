-- | Functional test for a run that should timeout.
module Timeout (spec) where

import Constants qualified
import Data.Text qualified as T
import ShellRun qualified as SR
import ShellRun.Parsing.Env qualified as Env
import ShellRun.Prelude
import System.Environment qualified as SysEnv
import System.IO.Silently qualified as Shh
import Test.Tasty (TestTree)
import Test.Tasty.HUnit qualified as THU
import Verify (ExpectedText (..), ResultText (..))
import Verify qualified as V

-- | Spec that should timeout.
spec :: TestTree
spec = THU.testCase "Should time out" $ do
  env <- SysEnv.withArgs argList Env.runParser
  let action = runReaderT (SR.runShellT SR.runShell) env
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
    <$> [ Constants.cancelled,
          Constants.totalTime
        ]
