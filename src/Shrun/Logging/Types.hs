{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the `Log` type and associated functions.
--
-- @since 0.3
module Shrun.Logging.Types
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

    -- * Log Queue
    LogText (.., MkLogText),
    LogTextQueue (..),
  )
where

import Optics.TH
  ( generateUpdateableOptics,
    makeFieldLabelsWith,
    noPrefixFieldLabels,
  )
import Shrun.Data.Command (Command)
import Shrun.Prelude
import System.Console.Pretty (Color (..))

-- | Determines the logging behavior.
--
-- @since 0.1
data LogMode
  = -- | Sets the logging region to this log.
    --
    -- @since 0.1
    LogModeSet
  | -- | Appends the log to the logging region.
    --
    -- @since 0.1
    LogModeAppend
  | -- | Closes the logging region, finishing with the log.
    --
    -- @since 0.1
    LogModeFinish
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

-- | Determines the logging level.
--
-- @since 0.1
data LogLevel
  = -- | @since 0.1
    LevelSubCommand
  | -- | @since 0.6.1
    LevelFinished
  | -- | @since 0.1
    LevelTimer
  | -- | @since 0.6.1
    LevelSuccess
  | -- | @since 0.6.1
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

-- | Determines where the log is sent.
--
-- @since 0.1
data LogDest
  = -- | @since 0.1
    LogDestConsole
  | -- | @since 0.1
    LogDestFile
  | -- | @since 0.1
    LogDestBoth
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
    -- either 'LogDestFile' or 'LogDestBoth' /and/ file logging must be enabled
    -- globally.
    --
    -- @since 0.1
    dest :: LogDest
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
levelToPrefix LevelSubCommand = "[Command]"
levelToPrefix LevelFinished = "[Finished]"
levelToPrefix LevelTimer = "[Timer]"
levelToPrefix LevelSuccess = "[Success]"
levelToPrefix LevelWarn = "[Warn]"
levelToPrefix LevelError = "[Error]"
levelToPrefix LevelFatal = "[Fatal]"
{-# INLINEABLE levelToPrefix #-}

-- | 'LogText' is a textual representation of a given 'Log'. No coloring
-- is included, but we include the prefix (e.g. Warn) along with a timestamp.
--
-- @since 0.1
newtype LogText = UnsafeLogText
  { -- | @since 0.1
    unLogText :: Text
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.6.1
makeFieldLabelsWith
  (noPrefixFieldLabels & generateUpdateableOptics .~ False)
  ''LogText

-- | @since 0.1
pattern MkLogText :: Text -> LogText
pattern MkLogText t <- UnsafeLogText t

{-# COMPLETE MkLogText #-}

-- | Newtype wrapper over a 'TBQueue'.
--
-- @since 0.1
newtype LogTextQueue = MkLogTextQueue
  { -- | @since 0.1
    getLogTextQueue :: TBQueue LogText
  }

-- | @since 0.6.1
makeFieldLabelsNoPrefix ''LogTextQueue

-- | @since 0.1
instance Show LogTextQueue where
  show _ = "<MkLogTextQueue>"
  {-# INLINEABLE show #-}
