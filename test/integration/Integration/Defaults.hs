module Integration.Defaults (specs) where

import Data.IORef qualified as IORef
import Integration.Prelude
import Integration.Utils
  ( SimpleEnv (..),
    makeEnvAndVerify,
    runConfigIO,
    runNoConfigIO,
  )
import Shrun.Configuration.Env.Types (CmdDisplay (..), StripControl (..))
import Shrun.Data.Command (Command (..))
import Shrun.Data.NonEmptySeq qualified as NESeq

specs :: IO TestArgs -> TestTree
specs testArgs =
  testGroup
    "Default configuration behavior"
    [ defaultEnv,
      usesDefaultConfigFile,
      cliOverridesConfigFile testArgs,
      ignoresDefaultConfigFile
    ]

defaultEnv :: TestTree
defaultEnv = testCase "No arguments and empty config path should return default Env" $ do
  logsRef <- IORef.newIORef []
  makeEnvAndVerify ["cmd1"] (`runNoConfigIO` logsRef) expected

  logs <- IORef.readIORef logsRef
  ["No default config found at: ./config.toml"] @=? logs
  where
    expected =
      MkSimpleEnv
        { timeout = Nothing,
          cmdLogging = False,
          cmdDisplay = ShowKey,
          cmdLogStripControl = Nothing,
          cmdLogNameTrunc = Nothing,
          cmdLogLineTrunc = Nothing,
          fileLogging = False,
          fileLogStripControl = Nothing,
          disableLogging = False,
          commands = NESeq.singleton "cmd1"
        }

usesDefaultConfigFile :: TestTree
usesDefaultConfigFile = testCase "No arguments should use config from default file" $ do
  logsRef <- IORef.newIORef []
  makeEnvAndVerify ["cmd1"] (`runConfigIO` logsRef) expected

  logs <- IORef.readIORef logsRef
  [] @=? logs
  where
    expected =
      MkSimpleEnv
        { timeout = Just 3_600,
          disableLogging = True,
          cmdDisplay = HideKey,
          cmdLogging = True,
          cmdLogStripControl = Just StripControlAll,
          cmdLogNameTrunc = Just 80,
          cmdLogLineTrunc = Just 150,
          fileLogging = True,
          fileLogStripControl = Just StripControlNone,
          commands = NESeq.singleton (MkCommand (Just "cmd1") "echo \"command one\"")
        }

cliOverridesConfigFile :: IO TestArgs -> TestTree
cliOverridesConfigFile testArgs = testCase "CLI args overrides config file" $ do
  logPath <- (</> "cli-log") . view #workingTmpDir <$> testArgs
  logsRef <- IORef.newIORef []

  makeEnvAndVerify (args logPath) (`runConfigIO` logsRef) expected
    `finally` deleteFileIfExists logPath

  logs <- IORef.readIORef logsRef
  logs @=? []
  where
    args logPath =
      [ "--config",
        "test/integration/toml/overridden.toml",
        "--timeout",
        "10",
        "--file-log",
        logPath,
        "--file-log-strip-control",
        "none",
        "--cmd-log",
        "--key-hide",
        "--cmd-name-trunc",
        "10",
        "--cmd-log-line-trunc",
        "60",
        "--cmd-log-strip-control",
        "none",
        "--log-disable",
        "cmd"
      ]
    expected =
      MkSimpleEnv
        { timeout = Just 10,
          disableLogging = True,
          cmdDisplay = HideKey,
          cmdLogging = True,
          cmdLogStripControl = Just StripControlNone,
          cmdLogNameTrunc = Just 10,
          cmdLogLineTrunc = Just 60,
          fileLogging = True,
          fileLogStripControl = Just StripControlNone,
          commands = NESeq.singleton "cmd"
        }

ignoresDefaultConfigFile :: TestTree
ignoresDefaultConfigFile = testCase "--no-config should ignore config file" $ do
  logsRef <- IORef.newIORef []
  makeEnvAndVerify ["--no-config", "cmd1"] (`runConfigIO` logsRef) expected

  logs <- IORef.readIORef logsRef
  logs @=? []
  where
    expected =
      MkSimpleEnv
        { timeout = Nothing,
          disableLogging = False,
          cmdDisplay = ShowKey,
          cmdLogging = False,
          cmdLogStripControl = Nothing,
          cmdLogNameTrunc = Nothing,
          cmdLogLineTrunc = Nothing,
          fileLogging = False,
          fileLogStripControl = Nothing,
          commands = NESeq.singleton "cmd1"
        }
