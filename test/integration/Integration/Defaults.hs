module Integration.Defaults (specs) where

import Integration.Prelude
import Integration.Utils (makeEnvAndVerify, _MkConfigIO, _MkNoConfigIO)
import Shrun.Configuration.Env
  ( CmdDisplay (..),
    CmdLogging (..),
    StripControl (..),
  )
import Shrun.Data.Command (Command (..))
import Shrun.Data.NonEmptySeq qualified as NESeq

specs :: TestTree
specs =
  testGroup
    "Default configuration behavior"
    [ defaultEnv,
      usesDefaultConfigFile,
      cliOverridesConfigFile,
      ignoresDefaultConfigFile
    ]

defaultEnv :: TestTree
defaultEnv = testCase "No arguments and empty config path should return default Env" $ do
  makeEnvAndVerify
    ["cmd1"]
    (view _MkNoConfigIO)
    Nothing
    Nothing
    StripControlAll
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
    (Just ())
    StripControlNone
    Enabled
    HideKey
    (Just 80)
    (Just 150)
    StripControlAll
    True
    (NESeq.singleton (MkCommand (Just "cmd1") "echo \"command one\""))

cliOverridesConfigFile :: TestTree
cliOverridesConfigFile =
  testCase "CLI args overrides config file" $
    do
      makeEnvAndVerify
        [ "--config",
          "test/integration/toml/overridden.toml",
          "--timeout",
          "10",
          "--file-log",
          "log",
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
        (view _MkConfigIO)
        (Just 10)
        (Just ())
        StripControlNone
        Enabled
        HideKey
        (Just 10)
        (Just 60)
        StripControlNone
        True
        (NESeq.singleton "cmd")
      `finally` deleteIfExists "log"

ignoresDefaultConfigFile :: TestTree
ignoresDefaultConfigFile = testCase "--no-config should ignore config file" $ do
  makeEnvAndVerify
    ["--no-config", "cmd1"]
    (view _MkConfigIO)
    Nothing
    Nothing
    StripControlAll
    Disabled
    ShowKey
    Nothing
    Nothing
    StripControlSmart
    False
    (NESeq.singleton "cmd1")
