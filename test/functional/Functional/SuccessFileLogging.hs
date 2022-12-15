-- | Functional test for a successful run that displays the key rather
-- than the command.
module Functional.SuccessFileLogging (spec) where

import Data.Text qualified as T
import Functional.Prelude
import Functional.TestArgs (TestArgs (..))
import Test.Shrun.Verifier (ExpectedText (..), ResultText (..))
import Test.Shrun.Verifier qualified as V

-- | Spec that should run commands displaying the key in the logs.
spec :: IO TestArgs -> TestTree
-- This looks a bit convoluted.
-- 1. We need to cleanup the output file, so we use Tasty's withResource
--    facility, even though there is no acquisition.
-- 2. The teardown needs our global TestArgs to get the working directory.
spec args = withResource (pure ()) (teardown args) $ \_ ->
  testGroup
    "File logging tests"
    [ fileLog args
    ]

fileLog :: IO TestArgs -> TestTree
fileLog args =
  testCase "Should write logs to file" $ do
    MkTestArgs {tmpDir} <- args
    let outpath = tmpDir </> outfile
        argList = ["--no-config", "-f" <> outpath, "sleep 2"]

    _ <- runAndGetLogs argList
    fileResult <- readFileUtf8Lenient outpath

    let results = MkResultText <$> T.lines fileResult
    V.verifyExpected results expected

expected :: List ExpectedText
expected =
  MkExpectedText
    <$> [ withSuccessPrefix "sleep 2"
        ]

outfile :: String
outfile = "log"

teardown :: IO TestArgs -> () -> IO ()
teardown args _ = do
  MkTestArgs {tmpDir} <- args
  void $ tryAny $ removeFileIfExists (tmpDir </> outfile)
