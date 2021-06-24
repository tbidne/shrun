{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functional test for a successful run.
module Success (spec) where

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
import Verify (ExpectedText (..), ResultText (..), UnexpectedText (..))
import Verify qualified as V

-- | Spec that should run commands successfully.
spec :: Spec
spec =
  Hspec.it "Should run commands successfully" $ do
    env <- SysEnv.withArgs argList Env.runParser
    let action = MTL.runReaderT (SR.runShellT SR.runShell) env
    result <- Shh.capture_ action
    let results = MkResultText <$> T.lines (T.pack result)
    V.verifyExpectedUnexpected results allExpected allUnexpected
  where
    argList = [legendPath, timeout] <> commands
    legendPath = "--legend=" <> Constants.workingDirectory <> "/output/legend.txt"
    commands = ["bad", "both", "echo hi"]
    timeout = "--timeout=5"

allExpected :: [ExpectedText]
allExpected =
  MkExpectedText
    <$> [ cmdEchoHi,
          cmdEcho1,
          cmdEchoLong,
          cmdBad,
          totalTime
        ]

allUnexpected :: [UnexpectedText]
allUnexpected =
  MkUnexpectedText
    <$> [ cmdEchoHiStdout
        ]

cmdBad :: Text
cmdBad = Constants.errPrefix "Error running `some nonsense`: /bin/sh: some: command not found. Time elapsed: "

cmdEchoHi :: Text
cmdEchoHi = Constants.infoSuccessPrefix "Successfully ran `echo hi`. Time elapsed: "

cmdEcho1 :: Text
cmdEcho1 = Constants.infoSuccessPrefix "Successfully ran `sleep 1 && echo 1`. Time elapsed: 1 second"

cmdEchoLong :: Text
cmdEchoLong = Constants.infoSuccessPrefix "Successfully ran `sleep 2 && echo long`. Time elapsed: 2 seconds"

cmdEchoHiStdout :: Text
cmdEchoHiStdout = "echo hi: hi"

totalTime :: Text
totalTime = Constants.totalTime "2 seconds"