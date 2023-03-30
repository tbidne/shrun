{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Integration.Failures (specs) where

import Control.Exception (IOException)
import Effects.Exception (StringException)
import Integration.Prelude
import Integration.Utils (runConfigIO)
import Shrun.Configuration.Env (withEnv)
import Shrun.Configuration.Legend (CyclicKeyError (..), DuplicateKeyError (..))

specs :: TestTree
specs =
  testGroup
    "Failures"
    ( [ missingConfig,
        duplicateKeys,
        emptyKey,
        emptyValue,
        cyclicKeys,
        emptyFileLog
      ]
        <> osxTests
    )

missingConfig :: TestTree
missingConfig = testCase "Missing explicit config throws exception" $ do
  logsRef <- newIORef []
  let args = ["-c", "bad-file.toml", "cmd"]
  result <- runCaptureError @IOException args logsRef

  case result of
    Nothing -> assertFailure "Expected exception"
    Just ex -> expectedErr @=? displayException ex

  logs <- readIORef logsRef
  logs @=? []
  where
    expectedErr = "bad-file.toml: withBinaryFile: does not exist (No such file or directory)"

duplicateKeys :: TestTree
duplicateKeys = testCase "Duplicate keys throws exception" $ do
  logsRef <- newIORef []
  let args = ["-c", getIntConfig "duplicate-keys", "cmd"]
  result <- runCaptureError args logsRef

  case result of
    Just (MkDuplicateKeyError k) ->
      "key1" @=? k
    Nothing -> assertFailure "Expected exception"

  logs <- readIORef logsRef
  logs @=? []

emptyKey :: TestTree
emptyKey = testCase "Empty key throws exception" $ do
  logsRef <- newIORef []
  let args = ["-c", getIntConfig "empty-key", "cmd"]
  result <- runCaptureError @TOMLError args logsRef

  case result of
    Just err -> expectedErr @=? displayException err
    Nothing -> assertFailure "Expected exception"

  logs <- readIORef logsRef
  logs @=? []
  where
    expectedErr = "Decode error at '.legend[0].key': Unexpected empty text"

emptyValue :: TestTree
emptyValue = testCase "Empty value throws exception" $ do
  logsRef <- newIORef []
  let args = ["-c", getIntConfig "empty-value", "cmd"]
  result <- runCaptureError @TOMLError args logsRef

  case result of
    Just err -> expectedErr @=? displayException err
    Nothing -> assertFailure "Exception exception"

  logs <- readIORef logsRef
  logs @=? []
  where
    expectedErr = "Decode error at '.legend[0].val': Unexpected empty text"

cyclicKeys :: TestTree
cyclicKeys = testCase "Cyclic keys throws exception" $ do
  logsRef <- newIORef []
  -- using config.toml, which has cyclic definition
  let args = ["a"]
  result <- runCaptureError args logsRef

  case result of
    Just (MkCyclicKeyError path) -> "a -> b -> c -> a" @=? path
    Nothing -> assertFailure "Exception exception"

  logs <- readIORef logsRef
  logs @=? []

emptyFileLog :: TestTree
emptyFileLog = testCase "Empty file log throws exception" $ do
  logsRef <- newIORef []
  let args = ["-c", getIntConfig "empty-file-log", "cmd"]
  result <- runCaptureError @TOMLError args logsRef

  case result of
    Just err -> expectedErr @=? displayException err
    Nothing -> assertFailure "Expected exception"

  logs <- readIORef logsRef
  logs @=? []
  where
    expectedErr = "Decode error at '.file-log.path': Empty path given for --file-log"

#if OSX
osxTests :: [TestTree]
osxTests =
  [ osxNotifyConfigError,
    osxNotifyArgError
  ]

osxNotifyConfigError :: TestTree
osxNotifyConfigError = testCase "OSX with notify config throws exception" $ do
  logsRef <- newIORef []
  -- Not getExampleConfigOS since we want to use the linux one w/ notify
  -- configuration
  let args = ["-c", getExampleConfig "config", "cmd"]
  result <- runCaptureError @StringException args logsRef

  case result of
    Just _ -> pure ()
    Nothing -> assertFailure "Expected exception"

osxNotifyArgError :: TestTree
osxNotifyArgError = testCase "OSX with notify arg throws exception" $ do
  logsRef <- newIORef []
  let args = ["--notify-action", "final" ,"cmd"]
  result <- runCaptureError @StringException args logsRef

  case result of
    Just _ -> pure ()
    Nothing -> assertFailure "Expected exception"
#else
osxTests :: [TestTree]
osxTests = []
#endif

runCaptureError :: (Exception e) => [String] -> IORef [Text] -> IO (Maybe e)
runCaptureError args logsRef =
  flip runConfigIO logsRef $
    withArgs args (withEnv pure $> Nothing)
      `catchCS` \(ex :: e) -> pure (Just ex)
