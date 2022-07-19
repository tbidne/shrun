module Integration.Failures (specs) where

import Integration.Prelude
import Integration.Utils (_MkConfigIO)
import ShellRun.Configuration.Env (makeEnv)
import ShellRun.Configuration.Legend (CyclicKeyError (..), LegendError (..))
import System.Environment (withArgs)

specs :: TestTree
specs =
  testGroup
    "ShellRun.Configuration.Env"
    [ missingConfig,
      duplicateKeys,
      emptyKey,
      emptyValue,
      cyclicKeys
    ]

missingConfig :: TestTree
missingConfig = testCase "Missing explicit config throws exception" $ do
  let args = ["-c", "bad-file.toml", "cmd"]
  result <-
    view _MkConfigIO $
      do
        withRunInIO (\runner -> withArgs args (runner makeEnv) $> Nothing)
        `catchIO` \e -> pure $ Just e

  case result of
    Nothing -> assertFailure "Exception exception"
    Just ex ->
      "bad-file.toml: withBinaryFile: does not exist (No such file or directory)"
        @=? displayException ex

duplicateKeys :: TestTree
duplicateKeys = testCase "Duplicate keys throws exception" $ do
  let args = ["-c", "test/integration/toml/duplicate-keys.toml", "cmd"]
  result <- view _MkConfigIO $ do
    withRunInIO (\runner -> withArgs args (runner makeEnv) $> Nothing)
      `catch` \e -> pure $ Just e

  case result of
    Just (LegendErrorDuplicateKeys k) ->
      "key1" @=? k
    Just other -> assertFailure $ "Unexpected exception: " <> show other
    Nothing -> assertFailure "Exception exception"

emptyKey :: TestTree
emptyKey = testCase "Empty key throws exception" $ do
  let args = ["-c", "test/integration/toml/empty-key.toml", "cmd"]
  result <- view _MkConfigIO $ do
    withRunInIO (\runner -> withArgs args (runner makeEnv) $> Nothing)
      `catch` \e -> pure $ Just e

  case result of
    Just (LegendErrorEmptyKey k) ->
      "=value" @=? k
    Just other -> assertFailure $ "Unexpected exception: " <> show other
    Nothing -> assertFailure "Exception exception"

emptyValue :: TestTree
emptyValue = testCase "Empty value throws exception" $ do
  let args = ["-c", "test/integration/toml/empty-value.toml", "cmd"]
  result <- view _MkConfigIO $ do
    withRunInIO (\runner -> withArgs args (runner makeEnv) $> Nothing)
      `catch` \e -> pure $ Just e

  case result of
    Just (LegendErrorEmptyValue v) ->
      "key=" @=? v
    Just other -> assertFailure $ "Unexpected exception: " <> show other
    Nothing -> assertFailure "Exception exception"

cyclicKeys :: TestTree
cyclicKeys = testCase "Cyclic keys throws exception" $ do
  -- using config.toml, which has cyclic definition
  let args = ["a"]
  result <- view _MkConfigIO $ do
    withRunInIO (\runner -> withArgs args (runner makeEnv) $> Nothing)
      `catch` \e -> pure $ Just e

  case result of
    Just (MkCyclicKeyError path) -> "a -> b -> c -> a" @=? path
    Nothing -> assertFailure "Exception exception"
