-- | Functional test for a successful run.
module Functional.SuccessNoLogging (spec) where

import Data.Text qualified as T
import Functional.Prelude
import ShellRun qualified as SR
import ShellRun.Env qualified as Env
import System.Environment qualified as SysEnv
import System.IO.Silently qualified as Shh
import Test.ShellRun.Verifier (ResultText (..), UnexpectedText (..))
import Test.ShellRun.Verifier qualified as V
import Test.Tasty.HUnit qualified as THU

-- | Spec that should run commands successfully.
spec :: TestTree
spec =
  THU.testCase "Should run commands successfully without logging" $ do
    let argList = ["--cmd-log", "--disable-log"] <> commands

    env <- SysEnv.withArgs argList Env.runParser
    let action = SR.runShellT SR.runShell env
    result <- Shh.capture_ action

    -- Sadly -- despite passing in --disable-log -- we do not receive an
    -- empty list result in this test (we do in the real app, modulo a newline,
    -- perhaps). Silently captures the test output itself e.g. the
    -- "Should run ..." description. Thus we have to manually verify that the
    -- real shell-run output is not present.
    let results = MkResultText <$> T.lines (T.pack result)
    V.verifyUnexpected results allUnexpected
  where
    commands = ["sleep 1 && echo hi && sleep 2"]

allUnexpected :: List UnexpectedText
allUnexpected =
  MkUnexpectedText
    <$> [ "Success",
          "Finished!"
        ]
