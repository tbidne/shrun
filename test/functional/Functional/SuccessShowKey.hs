{-# LANGUAGE QuasiQuotes #-}

-- | Functional test for a successful run that displays the key rather
-- than the command.
module Functional.SuccessShowKey (spec) where

import Data.Text qualified as T
import Functional.Prelude
import Functional.TestArgs (TestArgs (MkTestArgs, configPath, tmpDir))
import Test.Shrun.Verifier
  ( ExpectedText (MkExpectedText),
    ResultText (MkResultText),
    UnexpectedText (MkUnexpectedText),
  )
import Test.Shrun.Verifier qualified as V

-- | Spec that should run commands displaying the key in the logs.
spec :: IO TestArgs -> TestTree
spec args = withResource (pure ()) (teardown args) $ \_ ->
  testGroup
    "Show Key tests"
    [ showKey args,
      noShowKey args
    ]

showKey :: IO TestArgs -> TestTree
showKey args =
  testCase "Should show key rather than command" $ do
    withShowKey args True

noShowKey :: IO TestArgs -> TestTree
noShowKey args =
  testCase "Should show command rather than key" $ do
    withShowKey args False

withShowKey :: IO TestArgs -> Bool -> Assertion
withShowKey args addShowKey = do
  MkTestArgs {configPath, tmpDir} <- args
  let outpath = tmpDir </> outfile
      argList =
        [ "--config=" <> unsafeDecodeOsToFp configPath,
          showKeyArg,
          "-f" <> unsafeDecodeOsToFp outpath
        ]
          <> commands

  results <- fmap MkResultText <$> (readIORef =<< run argList)
  fileContents <- readFileUtf8Lenient outpath
  let fileResults = MkResultText <$> T.lines fileContents

  V.verifyExpectedUnexpected results expected unexpected
  V.verifyExpectedUnexpected fileResults expected unexpected
  where
    commands = ["both"]
    (showKeyArg, expected, unexpected) =
      if addShowKey
        then ("", showKeyExpected, showKeyUnexpected)
        else ("--log-key-hide", noShowKeyExpected, noShowKeyUnexpected)

showKeyExpected :: List ExpectedText
showKeyExpected =
  MkExpectedText
    <$> [ withSuccessPrefix "one",
          withSuccessPrefix "long"
        ]

showKeyUnexpected :: List UnexpectedText
showKeyUnexpected =
  MkUnexpectedText
    <$> [ withSuccessPrefix "sleep 1 && echo 1",
          withSuccessPrefix "sleep 2 && echo long"
        ]

noShowKeyExpected :: List ExpectedText
noShowKeyExpected =
  MkExpectedText
    <$> [ withSuccessPrefix "sleep 1 && echo 1",
          withSuccessPrefix "sleep 2 && echo long"
        ]

noShowKeyUnexpected :: List UnexpectedText
noShowKeyUnexpected =
  MkUnexpectedText
    <$> [ withSuccessPrefix "one",
          withSuccessPrefix "long"
        ]

outfile :: OsPath
outfile = [osp|show-key.log"|]

teardown :: IO TestArgs -> () -> IO ()
teardown args _ = do
  MkTestArgs {tmpDir} <- args
  void $ tryAny $ removeFileIfExists (tmpDir </> outfile)
