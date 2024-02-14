{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the `Log` type and associated functions.
module Shrun.Logging.Types
  ( -- * Basic Types
    Log (..),
    LogMode (..),
    LogLevel (..),

    -- * Console Logs
    ConsoleLog,
    LogRegion (..),

    -- * File Logs
    FileLog,

    -- * Misc
    defaultCmdLogSize,
  )
where

import Shrun.Data.Command (CommandP1)
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
  = LevelCommand
  | LevelFinished
  | LevelTimer
  | LevelSuccess
  | LevelWarn
  | LevelError
  | LevelFatal
  deriving stock (Bounded, Enum, Eq, Show)

-- | 'ConsoleLog' with possible region.
data LogRegion r
  = -- | Log with region.
    LogRegion LogMode r ConsoleLog
  | -- | Log without region.
    LogNoRegion ConsoleLog

-- | Captures the relevant information concerning a specific log
-- (i.e. command, text, level, and mode).
data Log = MkLog
  { -- | Optional command that produced this log.
    cmd :: Maybe CommandP1,
    -- | The 'Text' for a given log.
    msg :: Text,
    -- | The 'LogLevel' for a given log.
    lvl :: LogLevel,
    -- | The 'LogMode' for a given log.
    mode :: LogMode
  }
  deriving stock (Show)

makeFieldLabelsNoPrefix ''Log

defaultCmdLogSize :: Bytes B Natural
defaultCmdLogSize = MkBytes 1024
