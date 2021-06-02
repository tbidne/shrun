{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides the `MonadLogger` class, used for having pretty logs in
-- a monadic setting. Logging via this class includes both textual
-- prefixes (e.g. @[INFO]@, @[ERROR]@) and also common terminal control
-- prefixes for colors.
module ShellRun.Class.MonadLogger
  ( -- * Types for logging
    MonadLogger (..),
    LogLevel (..),
    LogMode (..),

    -- * Pretty formatted logging
    logLevelMode,
    logDebug,
    logInfo,
    logInfoBlue,
    logInfoCyan,
    logInfoSuccess,
    logWarn,
    logError,
    logFatal,

    -- * Functions for manipulating carriage returns
    resetCR,
    clearLine,
    clearNoLine,
  )
where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader qualified as MTL
import Data.Text (Text)
import Data.Text qualified as T
import System.Console.Pretty (Color)
import System.Console.Pretty qualified as P
import System.IO qualified as IO

-- | `MonadLogger` is a simple typeclass for abstracting logging functions.
class Monad m => MonadLogger m where
  logNoLine :: Text -> m ()
  logLine :: Text -> m ()

instance MonadLogger IO where
  -- NOTE: We want to force printing with flush.
  logNoLine txt = putStr (T.unpack txt) *> IO.hFlush IO.stdout
  logLine = putStrLn . T.unpack

instance MonadLogger m => MonadLogger (ReaderT e m) where
  logNoLine = MTL.lift . logNoLine
  logLine = MTL.lift . logLine

-- | Determines the logging newline behavior.
data LogMode
  = Line
  | NoLine

-- | Determines the logging level.
data LogLevel
  = Debug
  | Info
  | InfoBlue
  | InfoCyan
  | InfoSuccess
  | Warn
  | Error
  | Fatal
  deriving (Show)

levelToColor :: LogLevel -> Color
levelToColor Debug = P.White
levelToColor Info = P.White
levelToColor InfoBlue = P.Blue
levelToColor InfoCyan = P.Cyan
levelToColor InfoSuccess = P.Green
levelToColor Warn = P.Magenta
levelToColor Error = P.Red
levelToColor Fatal = P.Red

levelToPrefix :: LogLevel -> Text
levelToPrefix Debug = "[Debug] "
levelToPrefix Info = "[Info] "
levelToPrefix InfoBlue = "[Info] "
levelToPrefix InfoCyan = "[Info] "
levelToPrefix InfoSuccess = "[Info] "
levelToPrefix Warn = "[Warn] "
levelToPrefix Error = "[Error] "
levelToPrefix Fatal = "[Fatal Error] "

-- | This is the most general way to log, includes options for
-- `LogLevel` and `LogMode`.
logLevelMode :: MonadLogger m => LogLevel -> LogMode -> Text -> m ()
logLevelMode l t = logFn . P.color color . (<>) prefix
  where
    color = levelToColor l
    prefix = levelToPrefix l
    logFn = case t of
      Line -> logLine
      NoLine -> logNoLine

-- | Resets the carriage return.
resetCR :: MonadLogger m => m ()
resetCR = logNoLine "\r"

-- | Clears 80 characters on the given line, then prints a newline.
clearLine :: MonadLogger m => m ()
clearLine = do
  resetCR
  logLine spaces
  where
    spaces = T.pack $ replicate 80 ' '

-- | Clears the current line without starting a new one.
clearNoLine :: MonadLogger m => m ()
clearNoLine = do
  resetCR
  logNoLine spaces
  resetCR
  where
    spaces = T.pack $ replicate 80 ' '

-- | Debug formatted 'logLine'.
logDebug :: MonadLogger m => Text -> m ()
logDebug = logLevelMode Debug Line

-- | Info formatted 'logLine'.
logInfo :: MonadLogger m => Text -> m ()
logInfo = logLevelMode Info Line

-- | Blue Info formatted 'logLine'.
logInfoBlue :: MonadLogger m => Text -> m ()
logInfoBlue = logLevelMode InfoBlue Line

-- | Cyan Info formatted 'logLine'.
logInfoCyan :: MonadLogger m => Text -> m ()
logInfoCyan = logLevelMode InfoCyan Line

-- | Success Info formatted 'logLine'.
logInfoSuccess :: MonadLogger m => Text -> m ()
logInfoSuccess = logLevelMode InfoSuccess Line

-- | Warn formatted 'logLine'.
logWarn :: MonadLogger m => Text -> m ()
logWarn = logLevelMode Warn Line

-- | Error formatted 'logLine'.
logError :: MonadLogger m => Text -> m ()
logError = logLevelMode Error Line

-- | Fatal formatted 'logLine'.
logFatal :: MonadLogger m => Text -> m ()
logFatal = logLevelMode Fatal Line