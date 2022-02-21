-- | Functional test for a successful run with native logging.
module SuccessCommandLogging (spec) where

import Data.Text qualified as T
import ShellRun qualified as SR
import ShellRun.Env qualified as Env
import ShellRun.Prelude
import System.Directory qualified as Dir
import System.Environment qualified as SysEnv
import System.FilePath ((</>))
import System.IO.Silently qualified as Shh
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as THU
import TestArgs (TestArgs (..))
import Utils qualified as U
import Verify (ExpectedText (..), ResultText (..))
import Verify qualified as V

-- | Spec that should run commands successfully and print stdout.
spec :: IO TestArgs -> TestTree
spec args = Tasty.withResource (pure ()) (teardown args) $ \_ ->
  Tasty.testGroup
    "Command logging tests"
    [ THU.testCase "Should print commands stdout" $ do
        MkTestArgs {tTmpDir} <- args
        let outpath = tTmpDir </> outfile
            argList =
              [ -- Need file logging to read commands
                "-f" <> outpath,
                "sleep 2",
                cmdLogging
              ]
                <> commands

        env <- SysEnv.withArgs argList Env.runParser
        let action = runReaderT (SR.runShellT SR.runShell) env
        _ <- Shh.capture_ action

        fileResult <- readFileUtf8Lenient outpath
        let results = MkResultText <$> T.lines fileResult
        V.verifyExpected results expected
    ]
  where
    -- `sleep 1` is because commands that run too quickly will not have
    -- output logged
    commands = ["echo hi && sleep 1"]
    cmdLogging = "--cmd-log"

expected :: List ExpectedText
expected =
  MkExpectedText
    <$> [ U.subCommandPrefix "echo hi && sleep 1" "hi"
        ]

outfile :: List Char
outfile = "cmd_logs.txt"

teardown :: IO TestArgs -> () -> IO ()
teardown args _ = do
  MkTestArgs {tTmpDir} <- args
  Dir.removeFile (tTmpDir </> outfile)
