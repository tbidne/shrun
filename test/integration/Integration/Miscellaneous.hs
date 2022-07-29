module Integration.Miscellaneous (specs) where

import Data.IORef qualified as IORef
import Data.Text qualified as T
import Integration.Prelude
import Integration.Utils (runConfigIO)
import Numeric.Algebra (zero)
import Shrun.Configuration.Env (withEnv)
import Shrun.Effects.FileSystemReader

specs :: IO TestArgs -> TestTree
specs testArgs =
  testGroup
    "Miscellaneous"
    [ logFileWarn testArgs,
      logFileDelete testArgs
    ]

logFileWarn :: IO TestArgs -> TestTree
logFileWarn testArgs = testCase "Large log file should print warning" $ do
  logPath <- (</> "large-file-warn") . view #workingTmpDir <$> testArgs
  logsRef <- IORef.newIORef []
  let contents = T.replicate 1_500 "test "

      run = do
        writeFileUtf8 logPath contents

        flip runConfigIO logsRef $
          withRunInIO (\runner -> withArgs (args logPath) (runner (withEnv pure)))

  run `finally` deleteFileIfExists logPath

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

          flip runConfigIO logsRef $
            withRunInIO (\runner -> withArgs (args logPath) (runner (withEnv pure)))

          -- file should have been deleted then recreated with a file size of 0.
          getFileSize logPath

    size <- run `finally` deleteFileIfExists logPath
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
