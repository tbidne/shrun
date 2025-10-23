{-# LANGUAGE QuasiQuotes #-}

-- | Functional tests for readme examples.
module Functional.Examples.Core (tests) where

import Data.Text qualified as T
import Effects.FileSystem.PathReader (XdgDirectory (XdgState))
import Functional.Prelude
import Functional.TestArgs (TestArgs)
import Test.Shrun.Verifier qualified as V

-- NOTE: If tests in this module fail, fix then update configuration.md!

tests :: IO TestArgs -> TestTree
tests testArgs =
  testGroup
    "CoreConfig"
    [ initOn,
      initOff,
      legendKeysCacheTests testArgs,
      timeout
    ]

initOn :: TestTree
initOn =
  testCase "Runs init successful example" $ do
    results <- run args
    V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "--init",
          ". examples/bashrc",
          "bash_function"
        ]
    expected =
      [ withSuccessPrefix "bash_function",
        finishedPrefix
      ]

initOff :: TestTree
initOff =
  testCase "Runs init failure example" $ do
    results <- runExitFailure args
    V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "bash_function"
        ]
    expected =
      [ withErrorPrefix "bash_function",
        finishedPrefix
      ]

legendKeysCacheTests :: IO TestArgs -> TestTree
legendKeysCacheTests testArgs =
  testGroup
    "--legend-keys-cache"
    [ testLegendKeysAdd testArgs,
      testLegendKeysClear testArgs,
      testLegendKeysWrite testArgs,
      testLegendKeysOff testArgs
    ]

testLegendKeysAdd :: IO TestArgs -> TestTree
testLegendKeysAdd =
  testLegendKeysCache
    [osp|Add|]
    ("add", "add")
    (expected1, expected2)
  where
    expected1 = ["cfg1_key_1", "cfg1_key_2", "short"]
    expected2 = ["cfg1_key_1", "cfg1_key_2", "cfg2_key_1", "cfg2_key_2", "short"]

testLegendKeysClear :: IO TestArgs -> TestTree
testLegendKeysClear =
  testLegendKeysCache
    [osp|Clear|]
    ("add", "clear")
    (expected1, expected2)
  where
    expected1 = ["cfg1_key_1", "cfg1_key_2", "short"]
    expected2 = []

testLegendKeysWrite :: IO TestArgs -> TestTree
testLegendKeysWrite =
  testLegendKeysCache
    [osp|Write|]
    ("write", "write")
    (expected1, expected2)
  where
    expected1 = ["cfg1_key_1", "cfg1_key_2", "short"]
    expected2 = ["cfg2_key_1", "cfg2_key_2", "short"]

testLegendKeysOff :: IO TestArgs -> TestTree
testLegendKeysOff =
  testLegendKeysCache
    [osp|Off|]
    ("add", "off")
    (expected1, expected1)
  where
    expected1 = ["cfg1_key_1", "cfg1_key_2", "short"]

testLegendKeysCache ::
  OsPath ->
  (String, String) ->
  (List Text, List Text) ->
  IO TestArgs ->
  TestTree
testLegendKeysCache desc (action1, action2) (e1, e2) testArgs = testCase descStr $ do
  xdgDir <- (</> xdgName) . view #tmpDir <$> testArgs

  let env = mkEnv xdgDir
      keysPath = xdgDir </> [ospPathSep|shrun/legend-keys.txt|]

  void $ runFuncIO env args1
  contents1 <- readLines keysPath
  e1 @=? contents1

  void $ runFuncIO env args2
  contents2 <- readLines keysPath
  e2 @=? contents2
  where
    descStr = unsafeDecode desc

    xdgName = [osp|testLegendKeys|] <> desc

    (args1, args2) = mkLegendKeysCacheArgs

    mkLegendKeysCacheArgs = (mk action1 p1, mk action2 p2)
      where
        mk action cfgStr =
          [ "--config",
            "off",
            "--config",
            cfgStr,
            "--legend-keys-cache",
            action,
            "sleep 1"
          ]

        p1 = unsafeDecode $ mkPath [osp|cache1.toml|]
        p2 = unsafeDecode $ mkPath [osp|cache2.toml|]
        mkPath p = [ospPathSep|test/functional/|] </> p

mkEnv :: OsPath -> FuncIOEnv
mkEnv d =
  MkFuncIOEnv
    { xdgDir = Just $ \case
        XdgState -> d
        other -> error $ "Unexpected xdg: " ++ show other
    }

readLines :: OsPath -> IO (List Text)
readLines p = do
  exists <- doesFileExist p
  if exists
    then fmap T.lines . readFileUtf8ThrowM $ p
    else pure []

timeout :: TestTree
timeout =
  testCase "Runs timeout example" $ do
    results <- runExitFailure args
    V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "-t",
          "4",
          "sleep 2",
          "sleep 6",
          "sleep 8"
        ]
    expected =
      [ withSuccessPrefix "sleep 2",
        runningPrefix,
        "  - sleep 6",
        "  - sleep 8",
        timedOut,
        finishedPrefix
      ]
