{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Integration.Miscellaneous (specs) where

import Data.IORef qualified as IORef
import Data.Text qualified as T
import Effects.FileSystem.Utils qualified as FsUtils
import Integration.Prelude
import Integration.Utils
  ( SimpleEnv
      ( MkSimpleEnv,
        cmdLog,
        cmdLogLineTrunc,
        cmdLogSize,
        cmdLogStripControl,
        cmdNameTrunc,
        commands,
        fileLog,
        fileLogStripControl,
        init,
        keyHide,
        notifyAction,
        notifySystem,
        notifyTimeout,
        pollInterval,
        timeout,
        timerFormat
      ),
    makeEnvAndVerify,
    runConfigIO,
  )
import Shrun.Data.Command (Command (MkCommand))
import Shrun.Data.KeyHide (KeyHide (KeyHideOff, KeyHideOn))
import Shrun.Data.StripControl
  ( StripControl (StripControlAll, StripControlNone, StripControlSmart),
  )
import Shrun.Data.TimerFormat (TimerFormat (DigitalFull, ProseCompact))
import Shrun.Env (withEnv)
import Shrun.Notify.Types
  ( NotifyAction (NotifyAll, NotifyCommand),
    NotifySystem (AppleScript, DBus),
    NotifyTimeout (NotifyTimeoutNever),
  )

specs :: IO TestArgs -> TestTree
specs testArgs =
  testGroup
    "Miscellaneous"
    [ logFileWarn testArgs,
      logFileDelete testArgs,
      usesRecursiveCmdExample,
      usesRecursiveCmd,
      lineTruncDetect
    ]

logFileWarn :: IO TestArgs -> TestTree
logFileWarn testArgs = testPropertyNamed desc "logFileWarn" $ property $ do
  logPath <- liftIO $ (</> [osp|large-file-warn|]) . view #workingTmpDir <$> testArgs
  logsRef <- liftIO $ newIORef []
  let logsPathStr = FsUtils.unsafeDecodeOsToFp logPath
      contents = T.replicate 1_500 "test "

      run = liftIO $ do
        writeFileUtf8 logPath contents

        flip runConfigIO logsRef $ withArgs (args logsPathStr) (withEnv pure)

  run

  logs <- liftIO $ readIORef logsRef
  [warning logsPathStr] === logs
  where
    desc = "Large log file should print warning"
    warning fp =
      mconcat
        [ "Warning: log file '",
          T.pack fp,
          "' has size: 7.50 kb, ",
          "but specified threshold is: 5.50 kb."
        ]
    args fp =
      [ "-f",
        fp,
        "--file-log-size-mode",
        "warn 5.5 kb",
        "cmd"
      ]

logFileDelete :: IO TestArgs -> TestTree
logFileDelete testArgs = testPropertyNamed desc "logFileDelete" $ property $ do
  logPath <- liftIO $ (</> [osp|large-file-del|]) . view #workingTmpDir <$> testArgs
  logsRef <- liftIO $ newIORef []
  let logPathStr = FsUtils.unsafeDecodeOsToFp logPath
      contents = T.replicate 1_500 "test "

      run = liftIO $ do
        writeFileUtf8 logPath contents

        flip runConfigIO logsRef $ withArgs (args logPathStr) (withEnv pure)

        -- file should have been deleted then recreated with a file size of 0.
        getFileSize logPath

  size <- run
  0 === size

  logs <- liftIO $ readIORef logsRef
  [warning logPathStr] === logs
  where
    desc = "Large log file should be deleted"
    warning fp =
      mconcat
        [ "Warning: log file '",
          T.pack fp,
          "' has size: 7.50 kb, ",
          "but specified threshold is: 5.50 kb. Deleting log."
        ]
    args fp =
      [ "-f",
        fp,
        "--file-log-size-mode",
        "delete 5.5 kilobytes",
        "cmd"
      ]

{- ORMOLU_DISABLE -}

usesRecursiveCmdExample :: TestTree
usesRecursiveCmdExample = testPropertyNamed desc "usesRecursiveCmdExample"
  $ property
  $ do
    logsRef <- liftIO $ newIORef []
    makeEnvAndVerify args (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "Uses recursive command from example"
    args = ["multi1"]
    expected =
      MkSimpleEnv
        { timeout = Just 3600,
          init = Just ". some file",
          keyHide = KeyHideOn,
          pollInterval = 127,
          timerFormat = DigitalFull,
          cmdNameTrunc = Just 80,
          cmdLogSize = MkBytes 20,
          cmdLog = True,
          cmdLogStripControl = Just StripControlAll,
          cmdLogLineTrunc = Just 150,
          fileLog = True,
          fileLogStripControl = Just StripControlNone,
          notifyAction = Just NotifyAll,
#if OSX
          notifySystem = Just AppleScript,
#else
          notifySystem = Just (DBus ()),
#endif
          notifyTimeout = Just NotifyTimeoutNever,
          commands =
            MkCommand (Just "m1") "m1val"
              :<|| [ "m2",
                     "m3"
                   ]
        }

{- ORMOLU_ENABLE -}

usesRecursiveCmd :: TestTree
usesRecursiveCmd = testPropertyNamed desc "usesRecursiveCmd" $ property $ do
  logsRef <- liftIO $ newIORef []
  makeEnvAndVerify args (`runConfigIO` logsRef) expected

  logs <- liftIO $ readIORef logsRef
  [] === logs
  where
    desc = "Uses recursive commands"
    args = ["-c", getExampleConfig "default", "all", "echo cat"]
    expected =
      MkSimpleEnv
        { timeout = Nothing,
          init = Nothing,
          keyHide = KeyHideOff,
          pollInterval = 10_000,
          timerFormat = ProseCompact,
          cmdNameTrunc = Nothing,
          cmdLogSize = MkBytes 1024,
          cmdLog = False,
          cmdLogStripControl = Nothing,
          cmdLogLineTrunc = Nothing,
          fileLog = False,
          fileLogStripControl = Nothing,
          notifySystem = Nothing,
          notifyAction = Nothing,
          notifyTimeout = Nothing,
          commands =
            MkCommand (Just "cmd1") "echo \"command one\""
              :<|| [ MkCommand (Just "cmd4") "command four",
                     "echo hi",
                     "echo cat"
                   ]
        }

lineTruncDetect :: TestTree
lineTruncDetect = testPropertyNamed desc "lineTruncDetect" $ property $ do
  logsRef <- liftIO $ newIORef []
  makeEnvAndVerify args (`runConfigIO` logsRef) expected

  logs <- liftIO $ readIORef logsRef
  logs === []
  where
    desc = "cmdLogLineTrunc reads 'detect' string from toml"
    args = ["-c", getIntConfig "misc", "cmd1"]
    expected =
      MkSimpleEnv
        { timeout = Nothing,
          init = Nothing,
          cmdLog = True,
          keyHide = KeyHideOff,
          pollInterval = 10_000,
          cmdLogSize = MkBytes 1024,
          timerFormat = ProseCompact,
          cmdNameTrunc = Nothing,
          cmdLogStripControl = Just StripControlSmart,
          cmdLogLineTrunc = Just 87,
          fileLog = False,
          fileLogStripControl = Nothing,
          notifySystem = Nothing,
          notifyAction = Nothing,
          notifyTimeout = Nothing,
          commands = "cmd1" :<|| []
        }
