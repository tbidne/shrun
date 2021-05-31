{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun.Class.MonadLogger
  ( -- * Types for logging
    MonadLogger (..),
    LogLevel (..),
    LogMode (..),

    -- * Functions for manipulating carriage returns
    resetCR,
    clearLine,
    clearNoLine,

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
  )
where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader qualified as MTL
import Data.Text (Text)
import Data.Text qualified as T
import System.Console.Pretty (Color)
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

instance MonadLogger m => MonadLogger (ReaderT e m) where
  logNoLine = MTL.lift . logNoLine
  logLine = MTL.lift . logLine

data LogMode
  = Line
  | NoLine

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
levelToPrefix Fatal = "[Fatal] "

logLevelMode :: MonadLogger m => LogLevel -> LogMode -> Text -> m ()
logLevelMode l t = logFn . P.color color . (<>) prefix
  where
    color = levelToColor l
    prefix = levelToPrefix l
    logFn = case t of
      Line -> logLine
      NoLine -> logNoLine

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