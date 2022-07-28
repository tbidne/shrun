module Integration.Miscellaneous (specs) where

import Data.IORef qualified as IORef
import Data.Text qualified as T
import Integration.Prelude
import Integration.Utils (runConfigIO)
import Numeric.Algebra (zero)
import Shrun.Configuration.Env (withEnv)
import Shrun.Effects.FileSystemReader
import System.Environment (withArgs)

specs :: TestTree
specs =
  testGroup
    "Miscellaneous"
    [ logFileWarn,
      logFileDelete
    ]

logFileWarn :: TestTree
logFileWarn = testCase "Large log file should print warning" $ do
  logsRef <- IORef.newIORef []
  let contents = T.replicate 1_500 "test "
  writeFileUtf8 "large-file-warn" contents

  _ <-
    ( flip runConfigIO logsRef $
        withRunInIO (\runner -> withArgs args (runner (withEnv pure)))
      )
      `finally` deleteIfExists "large-file-warn"

  logs <- IORef.readIORef logsRef
  logs @=? [warning]
  where
    warning =
      mconcat
        [ "Warning: log file 'large-file-warn' has size: 7.50 kb, ",
          "but specified threshold is: 5.50 kb."
        ]
    args =
      [ "-f",
        "large-file-warn",
        "--file-log-size-mode",
        "warn 5.5 kb",
        "cmd"
      ]

logFileDelete :: TestTree
logFileDelete =
  testCase "Large log file should be deleted" $
    ( do
        logsRef <- IORef.newIORef []
        let contents = T.replicate 1_500 "test "
        writeFileUtf8 "large-file-del" contents

        _ <-
          ( flip runConfigIO logsRef $
              withRunInIO (\runner -> withArgs args (runner (withEnv pure)))
            )

        -- file should have been deleted then recreated with a file size of 0.
        size <- getFileSize "large-file-del"
        size @=? zero

        logs <- IORef.readIORef logsRef
        logs @=? [warning]
    )
      -- now delete this, after everything has run
      `finally` deleteIfExists "large-file-del"
  where
    warning =
      mconcat
        [ "Warning: log file 'large-file-del' has size: 7.50 kb, ",
          "but specified threshold is: 5.50 kb. Deleting log."
        ]
    args =
      [ "-f",
        "large-file-del",
        "--file-log-size-mode",
        "delete 5.5 kilobytes",
        "cmd"
      ]
