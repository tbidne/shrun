-- | Functional test for a successful run.
module Functional.Success (spec) where

import Functional.Prelude
import Functional.TestArgs (TestArgs (..))
import Test.Shrun.Verifier (ExpectedText (..), ResultText (..), UnexpectedText (..))
import Test.Shrun.Verifier qualified as V

-- | Spec that should run commands successfully.
spec :: IO TestArgs -> TestTree
spec args =
  testCase "Should run commands successfully" $ do
    MkTestArgs {configPath} <- args
    let argList =
          [ "--config",
            unsafeDecodeOsToFp configPath,
            "--key-hide",
            "--timeout",
            "5"
          ]
            <> commands

    results <- fmap MkResultText <$> (readIORef =<< runExitFailure argList)

    V.verifyExpectedUnexpected results allExpected allUnexpected
  where
    commands = ["bad", "both", "echo hi"]

allExpected :: List ExpectedText
allExpected =
  MkExpectedText
    <$> [ cmdEchoHi,
          cmdEcho1,
          cmdEchoLong,
          cmdBad,
          finishedPrefix
        ]

allUnexpected :: List UnexpectedText
allUnexpected =
  MkUnexpectedText
    <$> [ cmdEchoHiStdout
        ]

cmdBad :: (IsString s, Semigroup s) => s
cmdBad = withErrorPrefix "some nonsense"

cmdEchoHi :: Text
cmdEchoHi = withSuccessPrefix "echo hi"

cmdEcho1 :: Text
cmdEcho1 = withSuccessPrefix "sleep 1 && echo 1"

cmdEchoLong :: Text
cmdEchoLong = withSuccessPrefix "sleep 2 && echo long"

cmdEchoHiStdout :: Text
cmdEchoHiStdout = "echo hi: hi"
