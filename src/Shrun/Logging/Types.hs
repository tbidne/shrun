{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the `Log` type and associated functions.
module Shrun.Logging.Types
  ( -- * Basic Types
    Log (..),
    LogMessage (..),
    fromUnlined,
    unsafeMapLogMessage,
    LogMode (..),
    LogLevel (..),

    -- * Console Logs
    ConsoleLog,
    LogRegion (..),

    -- * File Logs
    FileLog,
  )
where

import Shrun.Command.Types (CommandP1)
import Shrun.Data.Text (UnlinedText)
import Shrun.Logging.Types.Internal
  ( ConsoleLog,
    FileLog,
    LogMode
      ( LogModeAppend,
        LogModeFinish,
        LogModeSet
      ),
  )
import Shrun.Prelude

-- | Determines the logging level.
data LogLevel
  = LevelDebug
  | LevelCommand
  | LevelFinished
  | LevelTimer
  | LevelSuccess
  | LevelWarn
  | LevelError
  | LevelFatal
  | LevelKilled
  deriving stock (Bounded, Enum, Eq, Show)

-- | 'ConsoleLog' with possible region.
data LogRegion r
  = -- | Log with region.
    LogRegion LogMode r ConsoleLog
  | -- | Log without region.
    LogNoRegion ConsoleLog

declareFieldLabels
  [d|
    newtype LogMessage = UnsafeLogMessage {unLogMessage :: Text}
      deriving stock (Eq, Show)
      deriving newtype (IsString)
    |]

makeFieldLabelsNoPrefix ''LogMessage

fromUnlined :: UnlinedText -> LogMessage
fromUnlined = UnsafeLogMessage . view #unUnlinedText

unsafeMapLogMessage :: (Text -> Text) -> LogMessage -> LogMessage
unsafeMapLogMessage f (UnsafeLogMessage m) = UnsafeLogMessage (f m)

-- | Captures the relevant information concerning a specific log
-- (i.e. command, text, level, and mode).
data Log = MkLog
  { -- | Optional command that produced this log.
    cmd :: Maybe CommandP1,
    -- | The 'Text' for a given log.
    msg :: LogMessage,
    -- | The 'LogLevel' for a given log.
    lvl :: LogLevel,
    -- | The 'LogMode' for a given log.
    mode :: LogMode
  }
  deriving stock (Show)

makeFieldLabelsNoPrefix ''Log
