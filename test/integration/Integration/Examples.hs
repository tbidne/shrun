module Integration.Examples (specs) where

import Data.IORef qualified as IORef
import Integration.Prelude
import Integration.Utils (SimpleEnv (..), makeEnvAndVerify, runConfigIO)
import Shrun.Configuration.Env.Types
  ( CmdDisplay (..),
    CmdLogging (..),
    StripControl (..),
  )
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
    args = ["-c", "examples/config.toml", "cmd"]
    expected =
      MkSimpleEnv
        { timeout = Just 20,
          fileLogging = True,
          fileLogStripControl = StripControlAll,
          cmdLogging = Enabled,
          cmdDisplay = ShowKey,
          cmdNameTrunc = Just 80,
          cmdLineTrunc = Just 150,
          stripControl = StripControlSmart,
          disableLogging = False,
          commands = NESeq.singleton "cmd"
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
          fileLogging = False,
          fileLogStripControl = StripControlAll,
          cmdLogging = Disabled,
          cmdDisplay = ShowKey,
          cmdNameTrunc = Nothing,
          cmdLineTrunc = Nothing,
          stripControl = StripControlSmart,
          disableLogging = False,
          commands = NESeq.singleton "cmd"
        }
