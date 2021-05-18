module ShellRunner.Logger
  ( -- * Basic logging functions
    logNoLine,
    logLine,

    -- * Functions for manipulating carriage returns
    resetCR,
    clearLine,

    -- * Pretty formatted logging
    logDebug,
    logInfo,
    logInfoBlue,
    logInfoCyan,
    logInfoSuccess,
    logWarn,
    logError,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import System.Console.Pretty qualified as P
import System.IO qualified as IO

-- | Logs without a newline character.
logNoLine :: Text -> IO ()
logNoLine txt = putStr (T.unpack txt) *> IO.hFlush IO.stdout

-- | Logs with a newline character.
logLine :: Text -> IO ()
logLine = putStrLn . T.unpack

-- | 'logNoLine' with a carriage return.
resetCR :: IO ()
resetCR = logNoLine "\r"

-- | 'resetCR' then `logLine` with 60 spaces.
clearLine :: IO ()
clearLine = do
  resetCR
  logLine spaces
  where
    spaces = T.pack $ replicate 80 ' '

-- | Debug formatted 'logLine'.
logDebug :: Text -> IO ()
logDebug = logLine . (<>) "[Debug] "

-- | Info formatted 'logLine'.
logInfo :: Text -> IO ()
logInfo = logLine . (<>) "[Info] "

-- | Blue Info formatted 'logLine'.
logInfoBlue :: Text -> IO ()
logInfoBlue = logLine . P.color P.Blue . (<>) "[Info] "

-- | Cyan Info formatted 'logLine'.
logInfoCyan :: Text -> IO ()
logInfoCyan = logLine . P.color P.Cyan . (<>) "[Info] "

-- | Success Info formatted 'logLine'.
logInfoSuccess :: Text -> IO ()
logInfoSuccess = logLine . P.color P.Green . (<>) "[Info] "

-- | Warn formatted 'logLine'.
logWarn :: Text -> IO ()
logWarn = logLine . P.color P.Magenta . (<>) "[Warn] "

-- | Error formatted 'logLine'.
logError :: Text -> IO ()
logError = logLine . P.color P.Red . (<>) "[Error] "