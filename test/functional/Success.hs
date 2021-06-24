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
          Constants.totalTime
        ]

allUnexpected :: [UnexpectedText]
allUnexpected =
  MkUnexpectedText
    <$> [ cmdEchoHiStdout
        ]

cmdBad :: Text
cmdBad = Constants.errPrefix "some nonsense"

cmdEchoHi :: Text
cmdEchoHi = Constants.infoSuccessPrefix "echo hi"

cmdEcho1 :: Text
cmdEcho1 = Constants.infoSuccessPrefix "sleep 1 && echo 1"

cmdEchoLong :: Text
cmdEchoLong = Constants.infoSuccessPrefix "sleep 2 && echo long"

cmdEchoHiStdout :: Text
cmdEchoHiStdout = "echo hi: hi"
