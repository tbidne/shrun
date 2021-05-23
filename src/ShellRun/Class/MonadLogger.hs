{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun.Class.MonadLogger
  ( -- * Class for logging
    MonadLogger (..),

    -- * Functions for manipulating carriage returns
    resetCR,
    clearLine,
    clearNoLine,

    -- * Pretty formatted logging
    logDebug,
    logInfo,
    logInfoBlue,
    logInfoCyan,
    logNoLineCyan,
    logInfoSuccess,
    logWarn,
    logError,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import System.Console.Pretty qualified as P
import System.IO qualified as IO

class Monad m => MonadLogger m where
  logNoLine :: Text -> m ()
  logLine :: Text -> m ()

instance MonadLogger IO where
  logNoLine :: Text -> IO ()
  logNoLine txt = putStr (T.unpack txt) *> IO.hFlush IO.stdout

  logLine :: Text -> IO ()
  logLine = putStrLn . T.unpack

-- | 'logNoLine' with a carriage return.
resetCR :: MonadLogger m => m ()
resetCR = logNoLine "\r"

-- | 'resetCR' then `logLine` with 80 spaces.
clearLine :: MonadLogger m => m ()
clearLine = do
  resetCR
  logLine spaces
  where
    spaces = T.pack $ replicate 80 ' '

-- | Clears the line and resets the carriage return.
clearNoLine :: MonadLogger m => m ()
clearNoLine = do
  resetCR
  logNoLine spaces
  resetCR
  where
    spaces = T.pack $ replicate 80 ' '

-- TODO: Refactor this module (e.g., log levels, general fn)
logNoLineCyan :: MonadLogger m => Text -> m ()
logNoLineCyan = logNoLine . P.color P.Cyan

-- | Debug formatted 'logLine'.
logDebug :: MonadLogger m => Text -> m ()
logDebug = logLine . (<>) "[Debug] "

-- | Info formatted 'logLine'.
logInfo :: MonadLogger m => Text -> m ()
logInfo = logLine . (<>) "[Info] "

-- | Blue Info formatted 'logLine'.
logInfoBlue :: MonadLogger m => Text -> m ()
logInfoBlue = logLine . P.color P.Blue . (<>) "[Info] "

-- | Cyan Info formatted 'logLine'.
logInfoCyan :: MonadLogger m => Text -> m ()
logInfoCyan = logLine . P.color P.Cyan . (<>) "[Info] "

-- | Success Info formatted 'logLine'.
logInfoSuccess :: MonadLogger m => Text -> m ()
logInfoSuccess = logLine . P.color P.Green . (<>) "[Info] "

-- | Warn formatted 'logLine'.
logWarn :: MonadLogger m => Text -> m ()
logWarn = logLine . P.color P.Magenta . (<>) "[Warn] "

-- | Error formatted 'logLine'.
logError :: MonadLogger m => Text -> m ()
logError = logLine . P.color P.Red . (<>) "[Error] "