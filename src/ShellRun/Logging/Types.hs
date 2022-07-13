{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the `Log` type and associated functions.
--
-- @since 0.3
module ShellRun.Logging.Types
  ( -- * Types for logging
    Log (..),
    LogMode (..),
    LogLevel (..),
    LogDest (..),

    -- * Utility functions for associate levels to colors/prefixes.
    logToColor,
    logToPrefix,
    levelToColor,
    levelToPrefix,
  )
where

import ShellRun.Command (Command)
import ShellRun.Data.Supremum (Supremum (..))
import ShellRun.Prelude
import System.Console.Pretty (Color)
import System.Console.Pretty qualified as P

-- | Determines the logging behavior.
--
-- @since 0.1
data LogMode
  = -- | Sets the logging region to this log.
    --
    -- @since 0.1
    Set
  | -- | Appends the log to the logging region.
    --
    -- @since 0.1
    Append
  | -- | Closes the logging region, finishing with the log.
    --
    -- @since 0.1
    Finish
  deriving stock
    ( -- | @since 0.1
      Bounded,
      -- | @since 0.1
      Enum,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.1
      Semigroup,
      -- | @since 0.1
      Monoid
    )
    via (Supremum LogMode)

-- | Determines the logging level.
--
-- @since 0.1
data LogLevel
  = -- | @since 0.1
    None
  | -- | @since 0.1
    SubCommand
  | -- | @since 0.1
    Debug
  | -- | @since 0.1
    Info
  | -- | @since 0.1
    InfoBlue
  | -- | @since 0.1
    InfoCyan
  | -- | @since 0.1
    InfoSuccess
  | -- | @since 0.1
    Warn
  | -- | @since 0.1
    Error
  | -- | @since 0.1
    Fatal
  deriving stock
    ( -- | @since 0.1
      Bounded,
      -- | @since 0.1
      Enum,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.1
      Semigroup,
      -- | @since 0.1
      Monoid
    )
    via (Supremum LogLevel)

-- | Determines where the log is sent.
--
-- @since 0.1
data LogDest
  = -- | @since 0.1
    LogConsole
  | -- | @since 0.1
    LogFile
  | -- | @since 0.1
    LogBoth
  deriving stock
    ( -- | @since 0.1
      Bounded,
      -- | @since 0.1
      Enum,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.1
      Semigroup,
      -- | @since 0.1
      Monoid
    )
    via (Supremum LogDest)

-- | Captures the relevant information concerning a specific log
-- (i.e. text, level, and mode).
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
    mode :: LogMode,
    -- | Where to send this log. Most logs should go to both the console and
    -- the file. For a log to actually be written to a file, 'dest' must be
    -- either 'LogFile' or 'LogBoth' /and/ file logging must be enabled
    -- globally.
    --
    -- @since 0.1
    dest :: LogDest
  }
  deriving stock
    ( -- | @since 0.1
      Show
    )

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
levelToColor None = P.White
levelToColor SubCommand = P.White
levelToColor Debug = P.White
levelToColor Info = P.Magenta
levelToColor InfoBlue = P.Blue
levelToColor InfoCyan = P.Cyan
levelToColor InfoSuccess = P.Green
levelToColor Warn = P.Yellow
levelToColor Error = P.Red
levelToColor Fatal = P.Red
{-# INLINEABLE levelToColor #-}

-- | Maps 'LogLevel' to \'Prefix\'.
--
-- @since 0.1
levelToPrefix :: LogLevel -> Text
levelToPrefix None = ""
levelToPrefix SubCommand = "[Command] "
levelToPrefix Debug = "[Debug] "
levelToPrefix Info = "[Info] "
levelToPrefix InfoBlue = "[Info] "
levelToPrefix InfoCyan = "[Info] "
levelToPrefix InfoSuccess = "[Info] "
levelToPrefix Warn = "[Warn] "
levelToPrefix Error = "[Error] "
levelToPrefix Fatal = "[Fatal Error] "
{-# INLINEABLE levelToPrefix #-}
