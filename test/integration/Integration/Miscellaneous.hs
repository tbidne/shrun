{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
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
import Shrun.Configuration.Env (withEnv)
import Shrun.Configuration.Env.Types
  ( KeyHide (KeyHideOff, KeyHideOn),
    StripControl (StripControlAll, StripControlNone, StripControlSmart),
  )
import Shrun.Data.Command (Command (MkCommand))
import Shrun.Data.TimerFormat (TimerFormat (DigitalFull, ProseCompact))
import Shrun.Notify.Types
  ( NotifyAction (NotifyCommand),
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
      usesRecursiveCmd
    ]

logFileWarn :: IO TestArgs -> TestTree
logFileWarn testArgs = testCase "Large log file should print warning" $ do
  logPath <- (</>! "large-file-warn") . view #workingTmpDir <$> testArgs
  logsRef <- IORef.newIORef []
  let logsPathStr = FsUtils.unsafeDecodeOsToFp logPath
      contents = T.replicate 1_500 "test "

      run = do
        writeFileUtf8 logPath contents

        flip runConfigIO logsRef $ withArgs (args logsPathStr) (withEnv pure)

  run `finally` removeFileIfExists logPath

  logs <- IORef.readIORef logsRef
  [warning logsPathStr] @=? logs
  where
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
logFileDelete testArgs =
  testCase "Large log file should be deleted" $ do
    logPath <- (</>! "large-file-del") . view #workingTmpDir <$> testArgs
    logsRef <- IORef.newIORef []
    let logPathStr = FsUtils.unsafeDecodeOsToFp logPath
        contents = T.replicate 1_500 "test "

        run = do
          writeFileUtf8 logPath contents

          flip runConfigIO logsRef $ withArgs (args logPathStr) (withEnv pure)

          -- file should have been deleted then recreated with a file size of 0.
          getFileSize logPath

    size <- run `finally` removeFileIfExists logPath
    0 @=? size

    logs <- IORef.readIORef logsRef
    [warning logPathStr] @=? logs
  where
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
usesRecursiveCmdExample = testCase "Uses recursive command from example" $ do
  logsRef <- IORef.newIORef []
  makeEnvAndVerify args (`runConfigIO` logsRef) expected

  logs <- IORef.readIORef logsRef
  logs @=? []
  where
    args = ["multi1"]
    expected =
      MkSimpleEnv
        { timeout = Just 3600,
          init = Just ". some file",
          keyHide = KeyHideOn,
          pollInterval = 127,
          timerFormat = DigitalFull,
          cmdNameTrunc = Just 80,
          cmdLog = True,
          cmdLogStripControl = Just StripControlAll,
          cmdLogLineTrunc = Just 150,
          fileLog = True,
          fileLogStripControl = Just StripControlNone,
          notifyAction = Just NotifyCommand,
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
usesRecursiveCmd = testCase "Uses recursive commands" $ do
  logsRef <- IORef.newIORef []
  makeEnvAndVerify args (`runConfigIO` logsRef) expected

  logs <- IORef.readIORef logsRef
  logs @=? []
  where
    args = ["-c", getExampleConfig "default", "all", "echo cat"]
    expected =
      MkSimpleEnv
        { timeout = Nothing,
          init = Nothing,
          keyHide = KeyHideOff,
          pollInterval = 10_000,
          timerFormat = ProseCompact,
          cmdNameTrunc = Nothing,
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
