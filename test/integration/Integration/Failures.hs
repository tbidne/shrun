{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Integration.Failures (specs) where

import Control.Exception (IOException)
import Data.List qualified as L
import Data.Text qualified as T
import Effects.Exception (StringException)
import Integration.Prelude
import Integration.Utils (runConfigIO)
import Shrun.Configuration.Env (withEnv)
import Shrun.Configuration.Legend
  ( CyclicKeyError (MkCyclicKeyError),
    DuplicateKeyError (MkDuplicateKeyError),
  )
import Shrun.Notify.Types (LinuxNotifySystemMismatch, OsxNotifySystemMismatch)

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
        <> osTests
    )

missingConfig :: TestTree
missingConfig = testCase "Missing explicit config throws exception" $ do
  logsRef <- newIORef []
  let args = ["-c", "bad-file.toml", "cmd"]
  result <- runCaptureError @IOException args logsRef

  case result of
    Nothing -> assertFailure "Expected exception"
    Just ex -> do
      let exMsg = displayException ex
      assertBool ("Exception: " ++ exMsg) (expectedStart `L.isPrefixOf` displayException ex)
      assertBool ("Exception: " ++ exMsg) (expectedEnd `L.isSuffixOf` displayException ex)

  logs <- readIORef logsRef
  logs @=? []
  where
    expectedStart = "bad-file.toml"
    expectedEnd = "does not exist (No such file or directory)"

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
osTests :: [TestTree]
osTests =
  [ osxNotifyConfigError,
    osxDBusError,
    osxNotifySendError
  ]

osxNotifyConfigError :: TestTree
osxNotifyConfigError = testCase "OSX with linux notify config throws exception" $ do
  logsRef <- newIORef []
  -- Not getExampleConfigOS since we want to use the linux one w/ notify
  -- configuration
  let args = ["-c", getExampleConfig "config", "cmd"]
  result <- runCaptureError @OsxNotifySystemMismatch args logsRef

  case result of
    Just ex -> exContains "Detected osx, but NotifySend is only available on linux!" ex
    Nothing -> assertFailure "Expected exception"

osxDBusError :: TestTree
osxDBusError = testCase "OSX with dbus throws exception" $ do
  logsRef <- newIORef []
  let args = ["--notify-system", "dbus" ,"cmd"]
  result <- runCaptureError @OsxNotifySystemMismatch args logsRef

  case result of
    Just ex -> exContains "Detected osx, but DBus is only available on linux!" ex
    Nothing -> assertFailure "Expected exception"

osxNotifySendError :: TestTree
osxNotifySendError = testCase "OSX with notify-send throws exception" $ do
  logsRef <- newIORef []
  let args = ["--notify-system", "notify-send" ,"cmd"]
  result <- runCaptureError @OsxNotifySystemMismatch args logsRef

  case result of
    Just ex -> exContains "Detected osx, but NotifySend is only available on linux!" ex
    Nothing -> assertFailure "Expected exception"
#else
osTests :: [TestTree]
osTests =
  [ linuxNotifyConfigError,
    linuxAppleScriptError
  ]

linuxNotifyConfigError :: TestTree
linuxNotifyConfigError = testCase "Linux with osx notify config throws exception" $ do
  logsRef <- newIORef []
  -- Not getExampleConfigOS since we want to use the linux one w/ notify
  -- configuration
  let args = ["-c", getExampleConfig "config_osx", "cmd"]
  result <- runCaptureError @LinuxNotifySystemMismatch args logsRef

  case result of
    Just ex -> exContains "Detected linux, but AppleScript is only available on osx!" ex
    Nothing -> assertFailure "Expected exception"

linuxAppleScriptError :: TestTree
linuxAppleScriptError = testCase "Linux with apple-script throws exception" $ do
  logsRef <- newIORef []
  let args = ["--notify-system", "apple-script" ,"cmd"]
  result <- runCaptureError @LinuxNotifySystemMismatch args logsRef

  case result of
    Just ex -> exContains "Detected linux, but AppleScript is only available on osx!" ex
    Nothing -> assertFailure "Expected exception"
#endif

runCaptureError :: (Exception e) => [String] -> IORef [Text] -> IO (Maybe e)
runCaptureError args logsRef =
  flip runConfigIO logsRef
    $ withArgs args (withEnv pure $> Nothing)
    `catchCS` \(ex :: e) -> pure (Just ex)

exContains :: (Exception e) => Text -> e -> Assertion
exContains txt ex = assertBool (T.unpack desc) . T.isInfixOf txt $ exTxt
  where
    desc =
      mconcat
        [ "Expected infix '",
          txt,
          "', received exception: '",
          exTxt,
          "'"
        ]
    exTxt = T.pack . displayException $ ex
