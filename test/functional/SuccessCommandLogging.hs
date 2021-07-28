-- | Functional test for a successful run with native logging.
module SuccessCommandLogging (spec) where

import Control.Monad.Reader qualified as MTL
import Data.Text qualified as T
import ShellRun qualified as SR
import ShellRun.Parsing.Env qualified as Env
import ShellRun.Prelude
import System.Environment qualified as SysEnv
import System.IO.Silently qualified as Shh
import Test.Hspec (Spec)
import Test.Hspec qualified as Hspec
import Verify (ExpectedText (..), ResultText (..))
import Verify qualified as V

-- | Spec that should run commands successfully and print stdout.
spec :: Spec
spec =
  Hspec.it "Should print commands stdout" $ do
    env <- SysEnv.withArgs argList Env.runParser
    let action = MTL.runReaderT (SR.runShellT SR.runShell) env
    result <- Shh.capture_ action
    let results = MkResultText <$> T.lines (T.pack result)
    V.verifyExpected results allExpected
  where
    argList = [commandLogging] <> commands
    -- `sleep 1` is because commands that run too quickly will not have
    -- output logged
    commands = ["echo hi && sleep 1"]
    commandLogging = "--command-logging"

allExpected :: [ExpectedText]
allExpected =
  MkExpectedText
    <$> [ "echo hi && sleep 1: hi"
        ]
