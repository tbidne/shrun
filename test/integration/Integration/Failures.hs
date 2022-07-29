module Integration.Failures (specs) where

import Data.IORef qualified as IORef
import Integration.Prelude
import Integration.Utils (runConfigIO)
import Shrun.Configuration.Env (TomlError (..), withEnv)
import Shrun.Configuration.Legend (CyclicKeyError (..), DuplicateKeyError (..))

specs :: TestTree
specs =
  testGroup
    "Failures"
    [ missingConfig,
      duplicateKeys,
      emptyKey,
      emptyValue,
      cyclicKeys,
      emptyFileLog
    ]

missingConfig :: TestTree
missingConfig = testCase "Missing explicit config throws exception" $ do
  logsRef <- IORef.newIORef []
  let args = ["-c", "bad-file.toml", "cmd"]
  result <-
    flip runConfigIO logsRef $
      withRunInIO (\runner -> withArgs args (runner (withEnv pure)) $> Nothing)
        `catchIO` \e -> pure $ Just e

  case result of
    Nothing -> assertFailure "Exception exception"
    Just ex -> expectedErr @=? displayException ex

  logs <- IORef.readIORef logsRef
  logs @=? []
  where
    expectedErr = "bad-file.toml: withBinaryFile: does not exist (No such file or directory)"

duplicateKeys :: TestTree
duplicateKeys = testCase "Duplicate keys throws exception" $ do
  logsRef <- IORef.newIORef []
  let args = ["-c", "test/integration/toml/duplicate-keys.toml", "cmd"]
  result <-
    flip runConfigIO logsRef $
      withRunInIO (\runner -> withArgs args (runner (withEnv pure)) $> Nothing)
        `catch` \e -> pure $ Just e

  case result of
    Just (MkDuplicateKeyError k) ->
      "key1" @=? k
    Nothing -> assertFailure "Expected exception"

  logs <- IORef.readIORef logsRef
  logs @=? []

emptyKey :: TestTree
emptyKey = testCase "Empty key throws exception" $ do
  logsRef <- IORef.newIORef []
  let args = ["-c", "test/integration/toml/empty-key.toml", "cmd"]
  result <-
    flip runConfigIO logsRef $
      withRunInIO (\runner -> withArgs args (runner (withEnv pure)) $> Nothing)
        `catch` \e -> pure $ Just e

  case result of
    Just err@(MkTomlError _) -> expectedErr @=? displayException err
    Nothing -> assertFailure "Expected exception"

  logs <- IORef.readIORef logsRef
  logs @=? []
  where
    expectedErr = "TOML error: Decode error at '.legend[0].key': Unexpected empty text"

emptyValue :: TestTree
emptyValue = testCase "Empty value throws exception" $ do
  logsRef <- IORef.newIORef []
  let args = ["-c", "test/integration/toml/empty-value.toml", "cmd"]
  result <-
    flip runConfigIO logsRef $
      withRunInIO (\runner -> withArgs args (runner (withEnv pure)) $> Nothing)
        `catch` \e -> pure $ Just e

  case result of
    Just err@(MkTomlError _) -> expectedErr @=? displayException err
    Nothing -> assertFailure "Exception exception"

  logs <- IORef.readIORef logsRef
  logs @=? []
  where
    expectedErr = "TOML error: Decode error at '.legend[0].val': Unexpected empty text"

cyclicKeys :: TestTree
cyclicKeys = testCase "Cyclic keys throws exception" $ do
  logsRef <- IORef.newIORef []
  -- using config.toml, which has cyclic definition
  let args = ["a"]
  result <-
    flip runConfigIO logsRef $
      withRunInIO (\runner -> withArgs args (runner (withEnv pure)) $> Nothing)
        `catch` \e -> pure $ Just e

  case result of
    Just (MkCyclicKeyError path) -> "a -> b -> c -> a" @=? path
    Nothing -> assertFailure "Exception exception"

  logs <- IORef.readIORef logsRef
  logs @=? []

emptyFileLog :: TestTree
emptyFileLog = testCase "Empty file log throws exception" $ do
  logsRef <- IORef.newIORef []
  let args = ["-c", "test/integration/toml/empty-file-log.toml", "cmd"]
  result <-
    flip runConfigIO logsRef $
      withRunInIO (\runner -> withArgs args (runner (withEnv pure)) $> Nothing)
        `catch` \e -> pure $ Just e

  case result of
    Just err@(MkTomlError _) -> expectedErr @=? displayException err
    Nothing -> assertFailure "Expected exception"

  logs <- IORef.readIORef logsRef
  logs @=? []
  where
    expectedErr = "TOML error: Decode error at '.file-log': Empty path given for --file-log"
