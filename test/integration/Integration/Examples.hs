{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Integration.Examples (specs) where

import Data.IORef qualified as IORef
import Integration.Prelude
import Integration.Utils (SimpleEnv (..), makeEnvAndVerify, runConfigIO)
import Shrun.Configuration.Env.Types (KeyHide (..), StripControl (..))
import Shrun.Data.Command (Command (MkCommand))
import Shrun.Notify.Types
  ( NotifyAction (..),
    NotifySystem (..),
    NotifyTimeout (..),
  )

specs :: TestTree
specs =
  testGroup
    "Examples"
    [ examplesConfig,
      examplesDefault
    ]

{- ORMOLU_DISABLE -}

examplesConfig :: TestTree
examplesConfig = testCase "examples/config.toml is valid" $ do
  logsRef <- IORef.newIORef []
  makeEnvAndVerify args (`runConfigIO` logsRef) expected

  logs <- IORef.readIORef logsRef
  logs @=? []
  where
    args = ["-c", getExampleConfigOS "config", "cmd1"]
    expected =
      MkSimpleEnv
        { timeout = Just 20,
          init = Just ". examples/bashrc",
          keyHide = KeyHideOff,
          pollInterval = 100,
          cmdNameTrunc = Just 80,
          cmdLog = True,
          cmdLogLineTrunc = Just 150,
          cmdLogStripControl = Just StripControlSmart,
          fileLog = False,
          fileLogStripControl = Nothing,
          notifyAction = Just NotifyCommand,
#if OSX
          notifySystem = Just AppleScript,
#else
          notifySystem = Just NotifySend,
#endif
          notifyTimeout = Just NotifyTimeoutNever,
          commands = MkCommand (Just "cmd1") "echo \"command one\"" :<|| []
        }

{- ORMOLU_ENABLE -}

examplesDefault :: TestTree
examplesDefault = testCase "examples/default.toml is valid" $ do
  logsRef <- IORef.newIORef []
  makeEnvAndVerify args (`runConfigIO` logsRef) expected

  logs <- IORef.readIORef logsRef
  logs @=? []
  where
    args = ["-c", getExampleConfig "default", "cmd"]
    expected =
      MkSimpleEnv
        { timeout = Nothing,
          init = Nothing,
          keyHide = KeyHideOff,
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
          commands = "cmd" :<|| []
        }
