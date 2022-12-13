module Integration.Examples (specs) where

import Data.IORef qualified as IORef
import Integration.Prelude
import Integration.Utils (SimpleEnv (..), makeEnvAndVerify, runConfigIO)
import Shrun.Configuration.Env.Types (CmdDisplay (..), StripControl (..))
import Shrun.Data.Command (Command (MkCommand))
import Shrun.Data.NonEmptySeq qualified as NESeq

specs :: TestTree
specs =
  testGroup
    "Examples"
    [ examplesConfig,
      examplesDefault
    ]

examplesConfig :: TestTree
examplesConfig = testCase "examples/config.toml is valid" $ do
  logsRef <- IORef.newIORef []
  makeEnvAndVerify args (`runConfigIO` logsRef) expected

  logs <- IORef.readIORef logsRef
  logs @=? []
  where
    args = ["-c", "examples/config.toml", "cmd1"]
    expected =
      MkSimpleEnv
        { timeout = Just 20,
          cmdDisplay = ShowKey,
          cmdLogging = True,
          cmdLogNameTrunc = Just 80,
          cmdLogLineTrunc = Just 150,
          cmdLogStripControl = Just StripControlSmart,
          fileLogging = True,
          fileLogStripControl = Just StripControlAll,
          commands = NESeq.singleton (MkCommand (Just "cmd1") "echo \"command one\"")
        }

examplesDefault :: TestTree
examplesDefault = testCase "examples/default.toml is valid" $ do
  logsRef <- IORef.newIORef []
  makeEnvAndVerify args (`runConfigIO` logsRef) expected

  logs <- IORef.readIORef logsRef
  logs @=? []
  where
    args = ["-c", "examples/default.toml", "cmd"]
    expected =
      MkSimpleEnv
        { timeout = Nothing,
          cmdDisplay = ShowKey,
          cmdLogging = False,
          cmdLogStripControl = Nothing,
          cmdLogNameTrunc = Nothing,
          cmdLogLineTrunc = Nothing,
          fileLogging = False,
          fileLogStripControl = Nothing,
          commands = NESeq.singleton "cmd"
        }
