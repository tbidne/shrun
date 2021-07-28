-- | Functional test for a successful run that displays the key rather
-- than the command.
module SuccessShowKey (spec) where

import Constants qualified
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

-- | Spec that should run commands displaying the key in the logs.
spec :: Spec
spec =
  Hspec.it "Should show key rather than command" $ do
    env <- SysEnv.withArgs argList Env.runParser
    let action = MTL.runReaderT (SR.runShellT SR.runShell) env
    result <- Shh.capture_ action
    let results = MkResultText <$> T.lines (T.pack result)
    V.verifyExpected results allExpected
  where
    argList = [legendPath, showKey, commandLogging] <> commands
    commands = ["short", "bad"]
    legendPath = "--legend=" <> Constants.workingDirectory <> "/output/legend.txt"
    showKey = "--show-key"
    commandLogging = "--command-logging"

allExpected :: [ExpectedText]
allExpected =
  MkExpectedText
    <$> [ showKeySuccess,
          showKeySublogging,
          showKeyError
        ]

showKeySuccess :: Text
showKeySuccess = Constants.infoSuccessPrefix "short"

showKeySublogging :: Text
showKeySublogging = Constants.subCommandPrefix "short: short"

showKeyError :: Text
showKeyError = "[Error] bad:"
