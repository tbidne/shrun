{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functional test for a successful run with native logging.
module SuccessSubLogging (spec) where

import Constants qualified
import Control.Monad.Reader qualified as MTL
import Data.Text (Text)
import Data.Text qualified as T
import ShellRun qualified as SR
import ShellRun.Parsing.Env qualified as Env
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
    argList = [nativeLog] <> commands
    -- `sleep 1` is because commands that run too quickly will not have
    -- output logged
    commands = ["echo hi && sleep 1"]
    nativeLog = "--sub-logging"

allExpected :: [ExpectedText]
allExpected =
  MkExpectedText
    <$> [ cmdEchoHiStdout
        ]

cmdEchoHiStdout :: Text
cmdEchoHiStdout = Constants.subCommandPrefix "echo hi && sleep 1: hi"