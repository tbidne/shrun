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

    -- * Utility functions for associate levels to colors/prefixes.
    logToColor,
    logToPrefix,
    levelToColor,
    levelToPrefix,
  )
where

import Shrun.Data.Command (Command)
import Shrun.Logging.Types.Internal (ConsoleLog, FileLog, LogMode (..))
import Shrun.Prelude
import System.Console.Pretty (Color (..))

-- NOTE: High-level picture of how logging works.
--
-- 1. Shrun.IO sends logs per command based on the environment (i.e. is file
--    logging on and/or do we log subcommands). If any logs are produced, they
--    are formatted and sent directly to a queue.
--
-- 2. Shrun also produces logs. These are based on "higher-level" logs, e.g.
--    success/failure status of a given command, fatal errors, etc. Shrun uses
--    functions in Shrun.Logging.Log that handles deciding if a given log
--    should be written to either/both of the console/file log queues.
--
-- 3. Shrun has two threads -- one for each queue -- that poll their
--    respective queues and writes logs as they are found. These do no
--    environment checking; any logs that make it to the queue are eventually
--    written.

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
    cmd :: Maybe Command,
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

-- | @since 0.1

-- | Transforms log to a color based on its 'LogLevel'.
--
-- @since 0.1
logToColor :: Log -> Color
logToColor = levelToColor . view #lvl
{-# INLINEABLE logToColor #-}

-- | Transforms log to a prefix based on its 'LogLevel'.
--
-- @since 0.1
logToPrefix :: Log -> Text
logToPrefix = levelToPrefix . view #lvl
{-# INLINEABLE logToPrefix #-}

-- | Maps 'LogLevel' to 'Color'.
--
-- @since 0.1
levelToColor :: LogLevel -> Color
levelToColor LevelSubCommand = White
levelToColor LevelFinished = Blue
levelToColor LevelTimer = Cyan
levelToColor LevelSuccess = Green
levelToColor LevelWarn = Yellow
levelToColor LevelError = Red
levelToColor LevelFatal = Red
{-# INLINEABLE levelToColor #-}

-- | Maps 'LogLevel' to \'Prefix\'.
--
-- @since 0.1
levelToPrefix :: LogLevel -> Text
levelToPrefix LevelSubCommand = "Command"
levelToPrefix LevelFinished = "Finished"
levelToPrefix LevelTimer = "Timer"
levelToPrefix LevelSuccess = "Success"
levelToPrefix LevelWarn = "Warn"
levelToPrefix LevelError = "Error"
levelToPrefix LevelFatal = "Fatal"
{-# INLINEABLE levelToPrefix #-}
