{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Integration.Defaults (specs) where

import Integration.Prelude
import Integration.Utils
  ( SimpleEnv (..),
    makeEnvAndVerify,
    runConfigIO,
    runNoConfigIO,
  )
import Shrun.Configuration.Env.Types (CmdDisplay (..), StripControl (..))
import Shrun.Data.Command (Command (..))
import Shrun.Notify.Types
  ( NotifyAction (..),
    NotifySystem (..),
    NotifyTimeout (..),
  )

specs :: IO TestArgs -> TestTree
specs testArgs =
  testGroup
    "Default configuration behavior"
    [ defaultEnv,
      usesDefaultConfigFile,
      cliOverridesConfigFile testArgs,
      cliOverridesConfigFileCmdLog,
      ignoresDefaultConfigFile,
      noXOverridesToml,
      noXOverridesArgs
    ]

defaultEnv :: TestTree
defaultEnv = testCase "No arguments and empty config path should return default Env" $ do
  logsRef <- newIORef []
  makeEnvAndVerify ["cmd1"] (`runNoConfigIO` logsRef) expected

  logs <- readIORef logsRef
  ["No default config found at: ./config.toml"] @=? logs
  where
    expected =
      MkSimpleEnv
        { timeout = Nothing,
          init = Nothing,
          cmdLog = False,
          keyHide = ShowKey,
          pollInterval = 10_000,
          cmdNameTrunc = Nothing,
          cmdLogStripControl = Nothing,
          cmdLogLineTrunc = Nothing,
          fileLog = False,
          fileLogStripControl = Nothing,
          notifySystem = Nothing,
          notifyAction = Nothing,
          notifyTimeout = Nothing,
          commands = "cmd1" :<|| []
        }

{- ORMOLU_DISABLE -}

usesDefaultConfigFile :: TestTree
usesDefaultConfigFile = testCase "No arguments should use config from default file" $ do
  logsRef <- newIORef []
  makeEnvAndVerify ["cmd1"] (`runConfigIO` logsRef) expected

  logs <- readIORef logsRef
  [] @=? logs
  where
    expected =
      MkSimpleEnv
        { timeout = Just 3_600,
          init = Just ". some file",
          keyHide = HideKey,
          pollInterval = 127,
          cmdNameTrunc = Just 80,
          cmdLog = True,
          cmdLogStripControl = Just StripControlAll,
          cmdLogLineTrunc = Just 150,
          fileLog = True,
          fileLogStripControl = Just StripControlNone,
#if OSX
          notifySystem = Nothing,
          notifyAction = Nothing,
          notifyTimeout = Nothing,
#else
          notifySystem = Just (DBus ()),
          notifyAction = Just NotifyCommand,
          notifyTimeout = Just NotifyTimeoutNever,
#endif
          commands = MkCommand (Just "cmd1") "echo \"command one\"" :<|| []
        }

cliOverridesConfigFile :: IO TestArgs -> TestTree
cliOverridesConfigFile testArgs = testCase "CLI args overrides config file" $ do
  logPath <- (</> "cli-log") . view #workingTmpDir <$> testArgs
  logsRef <- newIORef []

  makeEnvAndVerify (args logPath) (`runConfigIO` logsRef) expected
    `finally` removeFileIfExists logPath

  logs <- readIORef logsRef
  logs @=? []
  where
    args logPath =
      [ "--config",
        getIntConfigOS "overridden",
        "--timeout",
        "10",
        "--init",
        ". another file",
        "--file-log",
        logPath,
        "--file-log-strip-control",
        "none",
        "--cmd-log",
        "--key-hide",
        "--poll-interval",
        "127",
        "--cmd-name-trunc",
        "10",
        "--cmd-log-line-trunc",
        "60",
        "--cmd-log-strip-control",
        "none",
#if !OSX
        "--notify-system",
        "notify-send",
        "--notify-action",
        "final",
        "--notify-timeout",
        "10",
#endif
        "cmd"
      ]
    expected =
      MkSimpleEnv
        { timeout = Just 10,
          init = Just ". another file",
          keyHide = HideKey,
          pollInterval = 127,
          cmdNameTrunc = Just 10,
          cmdLog = True,
          cmdLogStripControl = Just StripControlNone,
          cmdLogLineTrunc = Just 60,
          fileLog = True,
          fileLogStripControl = Just StripControlNone,
#if OSX
          notifyAction = Nothing,
          notifySystem = Nothing,
          notifyTimeout = Nothing,
#else
          notifyAction = Just NotifyFinal,
          notifySystem = Just NotifySend,
          notifyTimeout = Just (NotifyTimeoutSeconds 10),
#endif
          commands = "cmd" :<|| []
        }

cliOverridesConfigFileCmdLog :: TestTree
cliOverridesConfigFileCmdLog = testCase desc $ do
  logsRef <- newIORef []

  makeEnvAndVerify args (`runConfigIO` logsRef) expected

  logs <- readIORef logsRef
  logs @=? []
  where
    desc = "CLI overrides config file cmd-log fields even when CLI --cmd-log is not specified"
    args =
      [ "--config",
        getIntConfigOS "overridden",
        "--cmd-log-line-trunc",
        "60",
        "--cmd-log-strip-control",
        "none",
        "cmd"
      ]
    expected =
      MkSimpleEnv
        { -- These two params we care about
          cmdLogStripControl = Just StripControlNone,
          cmdLogLineTrunc = Just 60,
          -- These are just the rest
          timeout = Just 3_600,
          init = Just "blah",
          keyHide = ShowKey,
          pollInterval = 10_000,
          cmdNameTrunc = Just 80,
          cmdLog = True,
          fileLog = True,
          fileLogStripControl = Just StripControlAll,
#if OSX
          notifyAction = Nothing,
          notifySystem = Nothing,
          notifyTimeout = Nothing,
#else
          notifyAction = Just NotifyCommand,
          notifySystem = Just (DBus ()),
          notifyTimeout = Just NotifyTimeoutNever,
#endif
          commands = "cmd" :<|| []
        }

{- ORMOLU_ENABLE -}

ignoresDefaultConfigFile :: TestTree
ignoresDefaultConfigFile = testCase "--no-config should ignore config file" $ do
  logsRef <- newIORef []
  makeEnvAndVerify ["--no-config", "cmd1"] (`runConfigIO` logsRef) expected

  logs <- readIORef logsRef
  logs @=? []
  where
    expected =
      MkSimpleEnv
        { timeout = Nothing,
          init = Nothing,
          keyHide = ShowKey,
          pollInterval = 10_000,
          cmdNameTrunc = Nothing,
          cmdLog = False,
          cmdLogStripControl = Nothing,
          cmdLogLineTrunc = Nothing,
          fileLog = False,
          fileLogStripControl = Nothing,
          notifySystem = Nothing,
          notifyAction = Nothing,
          notifyTimeout = Nothing,
          commands = "cmd1" :<|| []
        }

noXOverridesToml :: TestTree
noXOverridesToml = testCase "--no-x disables toml options" $ do
  logsRef <- newIORef []

  makeEnvAndVerify args (`runConfigIO` logsRef) expected

  logs <- readIORef logsRef
  logs @=? []
  where
    args =
      [ "--config",
        getIntConfigOS "overridden",
        "--no-timeout",
        "--no-init",
        "--no-key-hide",
        "--no-cmd-name-trunc",
        "--no-cmd-log",
        "--no-cmd-log-strip-control",
        "--no-cmd-log-line-trunc",
        "--no-file-log",
        "--no-file-log-strip-control",
        "--no-notify-action",
        "--no-notify-system",
        "--no-notify-timeout",
        "cmd"
      ]
    expected =
      MkSimpleEnv
        { timeout = Nothing,
          init = Nothing,
          keyHide = ShowKey,
          pollInterval = 10_000,
          cmdNameTrunc = Nothing,
          cmdLog = False,
          cmdLogStripControl = Nothing,
          cmdLogLineTrunc = Nothing,
          fileLog = False,
          fileLogStripControl = Nothing,
          notifyAction = Nothing,
          notifySystem = Nothing,
          notifyTimeout = Nothing,
          commands = "cmd" :<|| []
        }

noXOverridesArgs :: TestTree
noXOverridesArgs = testCase "--no-x disables args" $ do
  logsRef <- newIORef []

  makeEnvAndVerify args (`runConfigIO` logsRef) expected

  logs <- readIORef logsRef
  logs @=? []
  where
    args =
      [ "--timeout",
        "5",
        "--no-timeout",
        "--init",
        "blah",
        "--no-init",
        "--poll-interval",
        "555",
        "--no-poll-interval",
        "--key-hide",
        "--no-key-hide",
        "--cmd-name-trunc",
        "80",
        "--no-cmd-name-trunc",
        "--cmd-log",
        "--no-cmd-log",
        "--cmd-log-strip-control",
        "all",
        "--no-cmd-log-strip-control",
        "--cmd-log-line-trunc",
        "100",
        "--no-cmd-log-line-trunc",
        "--file-log",
        "path",
        "--no-file-log",
        "--file-log-strip-control",
        "all",
        "--no-file-log-strip-control",
        "--notify-action",
        "command",
        "--no-notify-action",
        "--notify-system",
        "dbus",
        "--no-notify-system",
        "--notify-timeout",
        "never",
        "--no-notify-timeout",
        "cmd"
      ]
    expected =
      MkSimpleEnv
        { timeout = Nothing,
          init = Nothing,
          keyHide = ShowKey,
          pollInterval = 10_000,
          cmdNameTrunc = Nothing,
          cmdLog = False,
          cmdLogStripControl = Nothing,
          cmdLogLineTrunc = Nothing,
          fileLog = False,
          fileLogStripControl = Nothing,
          notifyAction = Nothing,
          notifySystem = Nothing,
          notifyTimeout = Nothing,
          commands = "cmd" :<|| []
        }
