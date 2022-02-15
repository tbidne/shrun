-- | This module provides the `Log` type and associated functions.
--
-- @since 0.1.0.0
module ShellRun.Logging.Log
  ( -- * Types for logging
    Log (..),
    LogMode (..),
    LogLevel (..),

    -- * Helpers for creating logs
    logNone,
    logSubCommand,
    logDebug,
    logInfo,
    logInfoBlue,
    logInfoCyan,
    logInfoSuccess,
    logWarn,
    logError,
    logFatal,

    -- * Utility functions for associate levels to colors/prefixes.
    formatLog,
    formatLogNoColor,
    logToColor,
    logToPrefix,
    levelToColor,
    levelToPrefix,
  )
where

import ShellRun.Data.Supremum (Supremum (..))
import ShellRun.Prelude
import System.Console.Pretty (Color)
import System.Console.Pretty qualified as P

-- | Determines the logging behavior.
--
-- @since 0.1.0.0
data LogMode
  = -- | Sets the logging region to this log.
    --
    -- @since 0.1.0.0
    Set
  | -- | Appends the log to the logging region.
    --
    -- @since 0.1.0.0
    Append
  | -- | Closes the logging region, finishing with the log.
    --
    -- @since 0.1.0.0
    Finish
  deriving stock
    ( -- | @since 0.1.0.0
      Bounded,
      -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
  deriving
    ( -- | @since 0.1.0.0
      Semigroup,
      -- | @since 0.1.0.0
      Monoid
    )
    via (Supremum LogMode)

-- | Determines the logging level.
--
-- @since 0.1.0.0
data LogLevel
  = -- | @since 0.1.0.0
    None
  | -- | @since 0.1.0.0
    SubCommand
  | -- | @since 0.1.0.0
    Debug
  | -- | @since 0.1.0.0
    Info
  | -- | @since 0.1.0.0
    InfoBlue
  | -- | @since 0.1.0.0
    InfoCyan
  | -- | @since 0.1.0.0
    InfoSuccess
  | -- | @since 0.1.0.0
    Warn
  | -- | @since 0.1.0.0
    Error
  | -- | @since 0.1.0.0
    Fatal
  deriving stock
    ( -- | @since 0.1.0.0
      Bounded,
      -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
  deriving
    ( -- | @since 0.1.0.0
      Semigroup,
      -- | @since 0.1.0.0
      Monoid
    )
    via (Supremum LogLevel)

-- | Captures the relevant information concerning a specific log
-- (i.e. text, level, and mode).
--
-- @since 0.1.0.0
data Log = MkLog
  { -- | The 'Text' for a given log.
    --
    -- @since 0.1.0.0
    msg :: Text,
    -- | The 'LogLevel' for a given log.
    --
    -- @since 0.1.0.0
    lvl :: LogLevel,
    -- | The 'LogMode' for a given log.
    --
    -- @since 0.1.0.0
    mode :: LogMode
  }
  deriving stock
    ( -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
instance Semigroup Log where
  (<>) :: Log -> Log -> Log
  (MkLog t l m) <> (MkLog t' l' m') = MkLog (t <> t') (l <> l') (m <> m')

-- | @since 0.1.0.0
instance Monoid Log where
  mempty :: Log
  mempty = MkLog mempty mempty mempty

-- | Transforms log to a color based on its 'LogLevel'.
--
-- @since 0.1.0.0
logToColor :: Log -> Color
logToColor = levelToColor . lvl

-- | Transforms log to a prefix based on its 'LogLevel'.
--
-- @since 0.1.0.0
logToPrefix :: Log -> Text
logToPrefix = levelToPrefix . lvl

-- | Maps 'LogLevel' to 'Color'.
--
-- @since 0.1.0.0
levelToColor :: LogLevel -> Color
levelToColor None = P.White
levelToColor SubCommand = P.White
levelToColor Debug = P.White
levelToColor Info = P.White
levelToColor InfoBlue = P.Blue
levelToColor InfoCyan = P.Cyan
levelToColor InfoSuccess = P.Green
levelToColor Warn = P.Magenta
levelToColor Error = P.Red
levelToColor Fatal = P.Red

-- | Maps 'LogLevel' to \'Prefix\'.
--
-- @since 0.1.0.0
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

-- | Returns 'Log' with 'msg' = @txt@, 'lvl' = 'None', 'mode' = 'Set'.
--
-- @since 0.1.0.0
logNone :: Text -> Log
logNone txt = mempty {msg = txt, lvl = None}

-- | Returns 'Log' with 'msg' = @txt@, 'lvl' = 'SubCommand', 'mode' = 'Set'.
--
-- @since 0.1.0.0
logSubCommand :: Text -> Log
logSubCommand txt = mempty {msg = txt, lvl = SubCommand}

-- | Returns 'Log' with 'msg' = @txt@, 'lvl' = 'Debug', 'mode' = 'Set'.
--
-- @since 0.1.0.0
logDebug :: Text -> Log
logDebug txt = mempty {msg = txt, lvl = Debug}

-- | Returns 'Log' with 'msg' = @txt@, 'lvl' = 'Info', 'mode' = 'Set'.
--
-- @since 0.1.0.0
logInfo :: Text -> Log
logInfo txt = mempty {msg = txt, lvl = Info}

-- | Returns 'Log' with 'msg' = @txt@, 'lvl' = 'InfoBlue', 'mode' = 'Set'.
--
-- @since 0.1.0.0
logInfoBlue :: Text -> Log
logInfoBlue txt = mempty {msg = txt, lvl = InfoBlue}

-- | Returns 'Log' with 'msg' = @txt@, 'lvl' = 'InfoCyan', 'mode' = 'Set'.
--
-- @since 0.1.0.0
logInfoCyan :: Text -> Log
logInfoCyan txt = mempty {msg = txt, lvl = InfoCyan}

-- | Returns 'Log' with 'msg' = @txt@, 'lvl' = 'InfoSuccess', 'mode' = 'Set'.
--
-- @since 0.1.0.0
logInfoSuccess :: Text -> Log
logInfoSuccess txt = mempty {msg = txt, lvl = InfoSuccess}

-- | Returns 'Log' with 'msg' = @txt@, 'lvl' = 'Warn', 'mode' = 'Set'.
--
-- @since 0.1.0.0
logWarn :: Text -> Log
logWarn txt = mempty {msg = txt, lvl = Warn}

-- | Returns 'Log' with 'msg' = @txt@, 'lvl' = 'Error', 'mode' = 'Set'.
--
-- @since 0.1.0.0
logError :: Text -> Log
logError txt = mempty {msg = txt, lvl = Error}

-- | Returns 'Log' with 'msg' = @txt@, 'lvl' = 'Fatal', 'mode' = 'Set'.
--
-- @since 0.1.0.0
logFatal :: Text -> Log
logFatal txt = mempty {msg = txt, lvl = Fatal}

-- | Helper for turning a 'Log' into its own formatting 'Text', i.e., adding
-- a prefix and color.
--
-- @since 0.1.0.0
formatLog :: Log -> Text
formatLog log@MkLog {msg} = P.color color $ prefix <> msg
  where
    color = logToColor log
    prefix = logToPrefix log

-- | Helper for turning a 'Log' into its own formatting 'Text', i.e., adding
-- a prefix.
--
-- @since 0.1.0.0
formatLogNoColor :: Log -> Text
formatLogNoColor log@MkLog {msg} = prefix <> msg
  where
    prefix = logToPrefix log
