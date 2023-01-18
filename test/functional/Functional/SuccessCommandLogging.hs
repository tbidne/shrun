-- | Functional test for a successful run with native logging.
module Functional.SuccessCommandLogging (spec) where

import Data.Text qualified as T
import Functional.Prelude
import Functional.TestArgs (TestArgs (..))
import Test.Shrun.Verifier (ExpectedText (..), ResultText (..))
import Test.Shrun.Verifier qualified as V

-- | Spec that should run commands successfully and print stdout.
spec :: IO TestArgs -> TestTree
spec args = withResource (pure ()) (teardown args) $ \_ ->
  testGroup
    "Command logging tests"
    [ testCase "Should print commands stdout" $ do
        MkTestArgs {tmpDir} <- args
        let outpath = tmpDir </> outfile
            argList =
              [ -- Need file logging to read commands
                "-f" <> outpath,
                "--no-config",
                "sleep 2",
                cmdLogging
              ]
                <> commands

        _ <- run argList

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
    <$> [ withCommandPrefix "echo hi && sleep 1" "hi"
        ]

outfile :: String
outfile = "cmd_log"

teardown :: IO TestArgs -> () -> IO ()
teardown args _ = do
  MkTestArgs {tmpDir} <- args
  void $ tryAny $ removeFileIfExists (tmpDir </> outfile)
