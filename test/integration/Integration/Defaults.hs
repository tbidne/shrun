module Integration.Defaults (specs) where

import Integration.Prelude
import Integration.Utils (makeEnvAndVerify, _MkConfigIO, _MkNoConfigIO)
import ShellRun.Configuration.Env
  ( CmdDisplay (..),
    CmdLogging (..),
    StripControl (..),
  )
import ShellRun.Data.Command (Command (..))
import ShellRun.Data.NonEmptySeq qualified as NESeq

specs :: TestTree
specs =
  testGroup
    "Default configuration behavior"
    [ defaultEnv,
      usesDefaultConfigFile,
      cliOverridesConfigFile
    ]

defaultEnv :: TestTree
defaultEnv = testCase "No arguments and empty config path should return default Env" $ do
  makeEnvAndVerify
    ["cmd1"]
    (view _MkNoConfigIO)
    Nothing
    Nothing
    Disabled
    ShowKey
    Nothing
    Nothing
    StripControlSmart
    False
    (NESeq.singleton "cmd1")

usesDefaultConfigFile :: TestTree
usesDefaultConfigFile = testCase "No arguments should use config from default file" $ do
  makeEnvAndVerify
    ["cmd1"]
    (view _MkConfigIO)
    (Just 3_600)
    (Just "test/unit/Unit/toml/shell-run.log")
    Enabled
    HideKey
    (Just 80)
    (Just 150)
    StripControlAll
    True
    (NESeq.singleton (MkCommand (Just "cmd1") "echo \"command one\""))

cliOverridesConfigFile :: TestTree
cliOverridesConfigFile = testCase "CLI args overrides config file" $ do
  makeEnvAndVerify
    [ "--config",
      "test/unit/Unit/toml/overridden.toml",
      "--timeout",
      "10",
      "--file-log",
      "log",
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
    (view _MkConfigIO)
    (Just 10)
    (Just "log")
    Enabled
    HideKey
    (Just 10)
    (Just 60)
    StripControlNone
    True
    (NESeq.singleton "cmd")
