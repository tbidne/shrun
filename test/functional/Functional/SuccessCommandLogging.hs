-- | Functional test for a successful run with native logging.
module Functional.SuccessCommandLogging (spec) where

import Data.Text qualified as T
import Functional.Prelude
import Functional.TestArgs (TestArgs (..))
import Test.Shrun.Verifier (ExpectedText (..), ResultText (..))
import Test.Shrun.Verifier qualified as V

-- | Spec that should run commands successfully and print stdout.
spec :: IO TestArgs -> TestTree
spec args =
  testGroup
    "Command logging tests"
    [ success args,
      capturesStderr
    ]

success :: IO TestArgs -> TestTree
success args = withResource (pure ()) teardown $ \_ ->
  testCase "Should print commands stdout" $ do
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
  where
    -- `sleep 1` is because commands that run too quickly will not have
    -- output logged
    commands = ["echo hi && sleep 1"]
    cmdLogging = "--cmd-log"

    expected =
      MkExpectedText
        <$> [ withCommandPrefix "echo hi && sleep 1" "hi"
            ]

    outfile :: FilePath
    outfile = "cmd_log"

    teardown :: () -> IO ()
    teardown _ = do
      MkTestArgs {tmpDir} <- args
      void $ tryAny $ removeFileIfExists (tmpDir </> outfile)

capturesStderr :: TestTree
capturesStderr = testCase "Should capture stdout and stderr" $ do
  let argList =
        [ "--no-config",
          "--cmd-log",
          "./test/functional/Functional/stderr.sh"
        ]

  results <- fmap MkResultText <$> (readIORef =<< run argList)

  V.verifyExpected results expected
  where
    expected =
      [ withCommandPrefix "./test/functional/Functional/stderr.sh" "stdout 1",
        withCommandPrefix "./test/functional/Functional/stderr.sh" "stderr 1",
        withCommandPrefix "./test/functional/Functional/stderr.sh" "stdout 2",
        withCommandPrefix "./test/functional/Functional/stderr.sh" "stderr 2"
      ]