module Integration.Defaults (specs) where

import Data.IORef qualified as IORef
import Integration.Prelude
import Integration.Utils
  ( SimpleEnv (..),
    makeEnvAndVerify,
    runConfigIO,
    runNoConfigIO,
  )
import Shrun.Configuration.Env.Types
  ( CmdDisplay (..),
    CmdLogging (..),
    StripControl (..),
  )
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
  logs @=? ["No default config found at: ./config.toml"]
  where
    expected =
      MkSimpleEnv
        { timeout = Nothing,
          fileLog = False,
          fileLogStripControl = StripControlAll,
          cmdLogging = Disabled,
          cmdDisplay = ShowKey,
          cmdNameTrunc = Nothing,
          cmdLineTrunc = Nothing,
          stripControl = StripControlSmart,
          disableLogging = False,
          commands = NESeq.singleton "cmd1"
        }

usesDefaultConfigFile :: TestTree
usesDefaultConfigFile = testCase "No arguments should use config from default file" $ do
  logsRef <- IORef.newIORef []
  makeEnvAndVerify ["cmd1"] (`runConfigIO` logsRef) expected

  logs <- IORef.readIORef logsRef
  logs @=? []
  where
    expected =
      MkSimpleEnv
        { timeout = Just 3_600,
          fileLog = True,
          fileLogStripControl = StripControlNone,
          cmdLogging = Enabled,
          cmdDisplay = HideKey,
          cmdNameTrunc = Just 80,
          cmdLineTrunc = Just 150,
          stripControl = StripControlAll,
          disableLogging = True,
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
        "--cmd-line-trunc",
        "60",
        "--strip-control",
        "none",
        "--disable-log",
        "cmd"
      ]
    expected =
      MkSimpleEnv
        { timeout = Just 10,
          fileLog = True,
          fileLogStripControl = StripControlNone,
          cmdLogging = Enabled,
          cmdDisplay = HideKey,
          cmdNameTrunc = Just 10,
          cmdLineTrunc = Just 60,
          stripControl = StripControlNone,
          disableLogging = True,
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
          fileLog = False,
          fileLogStripControl = StripControlAll,
          cmdLogging = Disabled,
          cmdDisplay = ShowKey,
          cmdNameTrunc = Nothing,
          cmdLineTrunc = Nothing,
          stripControl = StripControlSmart,
          disableLogging = False,
          commands = NESeq.singleton "cmd1"
        }
