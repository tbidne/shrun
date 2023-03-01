{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the `Log` type and associated functions.
--
-- @since 0.3
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
  )
where

import Shrun.Data.Command (CommandP1)
import Shrun.Logging.Types.Internal (ConsoleLog, FileLog, LogMode (..))
import Shrun.Prelude

-- | Determines the logging level.
--
-- @since 0.1
data LogLevel
  = -- | @since 0.1
    LevelSubCommand
  | -- | @since 0.7
    LevelFinished
  | -- | @since 0.1
    LevelTimer
  | -- | @since 0.7
    LevelSuccess
  | -- | @since 0.7
    LevelWarn
  | -- | @since 0.1
    LevelError
  | -- | @since 0.1
    LevelFatal
  deriving stock
    ( -- | @since 0.1
      Bounded,
      -- | @since 0.1
      Enum,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | 'ConsoleLog' with possible region.
--
-- @since 0.7
data LogRegion r
  = -- | Log with region.
    --
    -- @since 0.7
    LogRegion LogMode r ConsoleLog
  | -- | Log without region.
    --
    -- @since 0.7
    LogNoRegion ConsoleLog

-- | Captures the relevant information concerning a specific log
-- (i.e. command, text, level, and mode).
--
-- @since 0.1
data Log = MkLog
  { -- | Optional command that produced this log.
    --
    -- @since 0.1
    cmd :: Maybe CommandP1,
    -- | The 'Text' for a given log.
    --
    -- @since 0.1
    msg :: Text,
    -- | The 'LogLevel' for a given log.
    --
    -- @since 0.1
    lvl :: LogLevel,
    -- | The 'LogMode' for a given log.
    --
    -- @since 0.1
    mode :: LogMode
  }
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''Log
