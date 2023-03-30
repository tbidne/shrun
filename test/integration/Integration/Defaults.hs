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
      ignoresDefaultConfigFile
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
          cmdLogging = False,
          cmdDisplay = ShowKey,
          pollInterval = 10_000,
          cmdLogStripControl = Nothing,
          cmdLogNameTrunc = Nothing,
          cmdLogLineTrunc = Nothing,
          fileLogging = False,
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
          cmdDisplay = HideKey,
          pollInterval = 127,
          cmdLogging = True,
          cmdLogStripControl = Just StripControlAll,
          cmdLogNameTrunc = Just 80,
          cmdLogLineTrunc = Just 150,
          fileLogging = True,
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
          cmdDisplay = HideKey,
          pollInterval = 127,
          cmdLogging = True,
          cmdLogStripControl = Just StripControlNone,
          cmdLogNameTrunc = Just 10,
          cmdLogLineTrunc = Just 60,
          fileLogging = True,
          fileLogStripControl = Just StripControlNone,
#if OSX
          notifySystem = Nothing,
          notifyAction = Nothing,
          notifyTimeout = Nothing,
#else
          notifySystem = Just NotifySend,
          notifyAction = Just NotifyFinal,
          notifyTimeout = Just (NotifyTimeoutSeconds 10),
#endif
          commands = "cmd" :<|| []
        }

{- ORMOLU_ENABLE -}

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
          cmdDisplay = ShowKey,
          pollInterval = 10_000,
          cmdLogging = True,
          cmdLogNameTrunc = Just 80,
          fileLogging = True,
          fileLogStripControl = Just StripControlAll,
          notifySystem = Nothing,
          notifyAction = Nothing,
          notifyTimeout = Nothing,
          commands = "cmd" :<|| []
        }

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
          cmdDisplay = ShowKey,
          pollInterval = 10_000,
          cmdLogging = False,
          cmdLogStripControl = Nothing,
          cmdLogNameTrunc = Nothing,
          cmdLogLineTrunc = Nothing,
          fileLogging = False,
          fileLogStripControl = Nothing,
          notifySystem = Nothing,
          notifyAction = Nothing,
          notifyTimeout = Nothing,
          commands = "cmd1" :<|| []
        }
