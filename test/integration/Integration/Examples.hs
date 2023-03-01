{-# LANGUAGE OverloadedLists #-}

module Integration.Examples (specs) where

import Data.IORef qualified as IORef
import Integration.Prelude
import Integration.Utils (SimpleEnv (..), makeEnvAndVerify, runConfigIO)
import Shrun.Configuration.Env.Types (CmdDisplay (..), StripControl (..))
import Shrun.Data.Command (Command (MkCommand))

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
          shellInit = Just ". examples/bashrc",
          cmdDisplay = ShowKey,
          pollInterval = 100,
          cmdLogging = True,
          cmdLogNameTrunc = Just 80,
          cmdLogLineTrunc = Just 150,
          cmdLogStripControl = Just StripControlSmart,
          fileLogging = False,
          fileLogStripControl = Nothing,
          commands = MkCommand (Just "cmd1") "echo \"command one\"" :<|| []
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
          shellInit = Nothing,
          cmdDisplay = ShowKey,
          pollInterval = 10_000,
          cmdLogging = False,
          cmdLogStripControl = Nothing,
          cmdLogNameTrunc = Nothing,
          cmdLogLineTrunc = Nothing,
          fileLogging = False,
          fileLogStripControl = Nothing,
          commands = "cmd" :<|| []
        }
