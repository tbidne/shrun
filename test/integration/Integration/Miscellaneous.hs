{-# LANGUAGE OverloadedLists #-}

module Integration.Miscellaneous (specs) where

import Data.IORef qualified as IORef
import Data.Text qualified as T
import Integration.Prelude
import Integration.Utils (SimpleEnv (..), makeEnvAndVerify, runConfigIO)
import Numeric.Algebra (zero)
import Shrun.Configuration.Env (withEnv)
import Shrun.Configuration.Env.Types (CmdDisplay (..), StripControl (..))
import Shrun.Data.Command (Command (..))

specs :: IO TestArgs -> TestTree
specs testArgs =
  testGroup
    "Miscellaneous"
    [ logFileWarn testArgs,
      logFileDelete testArgs,
      usesRecursiveCmdExample,
      usesRecursiveCmd
    ]

logFileWarn :: IO TestArgs -> TestTree
logFileWarn testArgs = testCase "Large log file should print warning" $ do
  logPath <- (</> "large-file-warn") . view #workingTmpDir <$> testArgs
  logsRef <- IORef.newIORef []
  let contents = T.replicate 1_500 "test "

      run = do
        writeFileUtf8 logPath contents

        flip runConfigIO logsRef $ withArgs (args logPath) (withEnv pure)

  run `finally` removeFileIfExists logPath

  logs <- IORef.readIORef logsRef
  [warning logPath] @=? logs
  where
    warning fp =
      mconcat
        [ "Warning: log file '",
          T.pack fp,
          "' has size: 7.50 kb, ",
          "but specified threshold is: 5.50 kb."
        ]
    args fp =
      [ "-f",
        fp,
        "--file-log-size-mode",
        "warn 5.5 kb",
        "cmd"
      ]

logFileDelete :: IO TestArgs -> TestTree
logFileDelete testArgs =
  testCase "Large log file should be deleted" $ do
    logPath <- (</> "large-file-del") . view #workingTmpDir <$> testArgs
    logsRef <- IORef.newIORef []
    let contents = T.replicate 1_500 "test "

        run = do
          writeFileUtf8 logPath contents

          flip runConfigIO logsRef $ withArgs (args logPath) (withEnv pure)

          -- file should have been deleted then recreated with a file size of 0.
          getFileSize logPath

    size <- run `finally` removeFileIfExists logPath
    zero @=? size

    logs <- IORef.readIORef logsRef
    [warning logPath] @=? logs
  where
    warning fp =
      mconcat
        [ "Warning: log file '",
          T.pack fp,
          "' has size: 7.50 kb, ",
          "but specified threshold is: 5.50 kb. Deleting log."
        ]
    args fp =
      [ "-f",
        fp,
        "--file-log-size-mode",
        "delete 5.5 kilobytes",
        "cmd"
      ]

usesRecursiveCmdExample :: TestTree
usesRecursiveCmdExample = testCase "Uses recursive command from example" $ do
  logsRef <- IORef.newIORef []
  makeEnvAndVerify args (`runConfigIO` logsRef) expected

  logs <- IORef.readIORef logsRef
  logs @=? []
  where
    args = ["multi1"]
    expected =
      MkSimpleEnv
        { timeout = Just 3600,
          init = Just ". some file",
          cmdDisplay = HideKey,
          pollInterval = 127,
          cmdLogging = True,
          cmdLogStripControl = Just StripControlAll,
          cmdLogNameTrunc = Just 80,
          cmdLogLineTrunc = Just 150,
          fileLogging = True,
          fileLogStripControl = Just StripControlNone,
          commands =
            MkCommand (Just "m1") "m1val"
              :<|| [ "m2",
                     "m3"
                   ]
        }

usesRecursiveCmd :: TestTree
usesRecursiveCmd = testCase "Uses recursive commands" $ do
  logsRef <- IORef.newIORef []
  makeEnvAndVerify args (`runConfigIO` logsRef) expected

  logs <- IORef.readIORef logsRef
  logs @=? []
  where
    args = ["-c", "examples/default.toml", "all", "echo cat"]
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
          commands =
            MkCommand (Just "cmd1") "echo \"command one\""
              :<|| [ MkCommand (Just "cmd4") "command four",
                     "echo hi",
                     "echo cat"
                   ]
        }
