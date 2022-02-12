-- | This module provides the `Log` type and associated functions.
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

-- | Determines the logging newline/carriage return behavior.
data LogMode
  = Set
  | Append
  | Finish
  deriving stock (Bounded, Eq, Ord, Show)
  deriving (Semigroup, Monoid) via (Supremum LogMode)

-- | Determines the logging level.
data LogLevel
  = None
  | SubCommand
  | Debug
  | Info
  | InfoBlue
  | InfoCyan
  | InfoSuccess
  | Warn
  | Error
  | Fatal
  deriving stock (Bounded, Eq, Ord, Show)
  deriving (Semigroup, Monoid) via (Supremum LogLevel)

-- | Captures the relevant information concerning a specific log
-- (i.e. text, level, and mode).
data Log = MkLog
  { -- | The 'Text' for a given log.
    msg :: Text,
    -- | The 'LogLevel' for a given log.
    lvl :: LogLevel,
    -- | The 'LogMode' for a given log.
    mode :: LogMode
  }
  deriving stock (Show)

instance Semigroup Log where
  (<>) :: Log -> Log -> Log
  (MkLog t l m) <> (MkLog t' l' m') = MkLog (t <> t') (l <> l') (m <> m')

instance Monoid Log where
  mempty :: Log
  mempty = MkLog mempty mempty mempty

-- | Transforms log to a color based on its 'LogLevel'.
logToColor :: Log -> Color
logToColor = levelToColor . lvl

-- | Transforms log to a prefix based on its 'LogLevel'.
logToPrefix :: Log -> Text
logToPrefix = levelToPrefix . lvl

-- | Maps 'LogLevel' to 'Color'.
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

-- | Returns 'Log' with 'msg' = @txt@, 'lvl' = 'None', 'mode' = 'NewLine'.
logNone :: Text -> Log
logNone txt = mempty {msg = txt, lvl = None}

-- | Returns 'Log' with 'msg' = @txt@, 'lvl' = 'SubCommand', 'mode' = 'NewLine'.
logSubCommand :: Text -> Log
logSubCommand txt = mempty {msg = txt, lvl = SubCommand}

-- | Returns 'Log' with 'msg' = @txt@, 'lvl' = 'Debug', 'mode' = 'NewLine'.
logDebug :: Text -> Log
logDebug txt = mempty {msg = txt, lvl = Debug}

-- | Returns 'Log' with 'msg' = @txt@, 'lvl' = 'Info', 'mode' = 'NewLine'.
logInfo :: Text -> Log
logInfo txt = mempty {msg = txt, lvl = Info}

-- | Returns 'Log' with 'msg' = @txt@, 'lvl' = 'InfoBlue', 'mode' = 'NewLine'.
logInfoBlue :: Text -> Log
logInfoBlue txt = mempty {msg = txt, lvl = InfoBlue}

-- | Returns 'Log' with 'msg' = @txt@, 'lvl' = 'InfoCyan', 'mode' = 'NewLine'.
logInfoCyan :: Text -> Log
logInfoCyan txt = mempty {msg = txt, lvl = InfoCyan}

-- | Returns 'Log' with 'msg' = @txt@, 'lvl' = 'InfoSuccess', 'mode' = 'NewLine'.
logInfoSuccess :: Text -> Log
logInfoSuccess txt = mempty {msg = txt, lvl = InfoSuccess}

-- | Returns 'Log' with 'msg' = @txt@, 'lvl' = 'Warn', 'mode' = 'NewLine'.
logWarn :: Text -> Log
logWarn txt = mempty {msg = txt, lvl = Warn}

-- | Returns 'Log' with 'msg' = @txt@, 'lvl' = 'Error', 'mode' = 'NewLine'.
logError :: Text -> Log
logError txt = mempty {msg = txt, lvl = Error}

-- | Returns 'Log' with 'msg' = @txt@, 'lvl' = 'Fatal', 'mode' = 'NewLine'.
logFatal :: Text -> Log
logFatal txt = mempty {msg = txt, lvl = Fatal}

-- | Helper for turning a 'Log' into its own formatting 'Text', i.e., adding
-- a prefix and color.
formatLog :: Log -> Text
formatLog log@MkLog {msg} = P.color color $ prefix <> msg
  where
    color = logToColor log
    prefix = logToPrefix log

-- | Helper for turning a 'Log' into its own formatting 'Text', i.e., adding
-- a prefix.
formatLogNoColor :: Log -> Text
formatLogNoColor log@MkLog {msg} = prefix <> msg
  where
    prefix = logToPrefix log
