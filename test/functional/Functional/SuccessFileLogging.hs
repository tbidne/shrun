-- | Functional test for a successful run that displays the key rather
-- than the command.
module Functional.SuccessFileLogging (spec) where

import Data.Text qualified as T
import Functional.Prelude
import Functional.TestArgs (TestArgs (..))
import Functional.Utils qualified as U
import System.Directory qualified as Dir
import System.FilePath ((</>))
import Test.ShellRun.Verifier (ExpectedText (..), ResultText (..))
import Test.ShellRun.Verifier qualified as V

-- | Spec that should run commands displaying the key in the logs.
spec :: IO TestArgs -> TestTree
-- This looks a bit convoluted.
-- 1. We need to cleanup the output file, so we use Tasty's withResource
--    facility, even though there is no acquisition.
-- 2. The teardown needs our global TestArgs to get the working directory.
spec args = withResource (pure ()) (teardown args) $ \_ ->
  testGroup
    "File logging tests"
    [ fileLogging args
    ]

fileLogging :: IO TestArgs -> TestTree
fileLogging args =
  testCase "Should write logs to file" $ do
    MkTestArgs {tmpDir} <- args
    let outpath = tmpDir </> outfile
        argList = ["--no-config", "-f" <> outpath, "sleep 2"]

    _ <- U.runAndGetLogs argList
    fileResult <- readFileUtf8Lenient outpath

    let results = MkResultText <$> T.lines fileResult
    V.verifyExpected results expected

expected :: List ExpectedText
expected =
  MkExpectedText
    <$> [ U.infoSuccessPrefix "sleep 2"
        ]

outfile :: List Char
outfile = "log"

teardown :: IO TestArgs -> () -> IO ()
teardown args _ = do
  MkTestArgs {tmpDir} <- args
  Dir.removeFile (tmpDir </> outfile)
