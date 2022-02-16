-- | Functional test for a successful run that displays the key rather
-- than the command.
module SuccessFileLogging (spec) where

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

-- | Spec that should run commands displaying the key in the logs.
spec :: IO TestArgs -> TestTree
-- This looks a bit convoluted.
-- 1. We need to cleanup the output file, so we use Tasty's withResource
--    facility, even though there is no acquisition.
-- 2. The teardown needs our global TestArgs to get the working directory.
spec args = Tasty.withResource (pure ()) (teardown args) $ \_ ->
  Tasty.testGroup
    "File logging tests"
    [ fileLogging args
    ]

fileLogging :: IO TestArgs -> TestTree
fileLogging args =
  THU.testCase "Should write logs to file" $ do
    MkTestArgs {tTmpDir} <- args
    let outpath = tTmpDir </> outfile
        argList = ["-f" <> outpath, "sleep 2"]

    env <- SysEnv.withArgs argList Env.runParser
    let action = runReaderT (SR.runShellT SR.runShell) env
    _ <- Shh.capture_ action
    fileResult <- readFileUtf8Lenient outpath

    let results = MkResultText <$> T.lines fileResult
    V.verifyExpected results expected

expected :: List ExpectedText
expected =
  MkExpectedText
    <$> [ U.infoSuccessPrefix "sleep 2"
        ]

outfile :: List Char
outfile = "logs.txt"

teardown :: IO TestArgs -> () -> IO ()
teardown args _ = do
  MkTestArgs {tTmpDir} <- args
  Dir.removeFile (tTmpDir </> outfile)
