{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal module for logging types.
module Shrun.Logging.Types.Internal
  ( FileLog (..),
    ConsoleLog (..),
    LogMode (..),
  )
where

import Shrun.Prelude

-- | 'FileLog' is a textual representation of a given log after it has
-- been formatted. No coloring is included, but we include the prefix
-- (e.g. Warn) along with a timestamp.
newtype FileLog = UnsafeFileLog
  { unFileLog :: Text
  }
  deriving stock (Eq, Show)

makeFieldLabelsWith
  (noPrefixFieldLabels & generateUpdateableOptics .~ False)
  ''FileLog

-- | 'ConsoleLog' is a textual representation of a given log after it has
-- been formatted.
newtype ConsoleLog = UnsafeConsoleLog
  { unConsoleLog :: Text
  }
  deriving stock (Eq, Show)

makeFieldLabelsWith
  (noPrefixFieldLabels & generateUpdateableOptics .~ False)
  ''ConsoleLog

-- NOTE: LogMode exists here so we do not have cyclic dependencies w/
-- RegionLogger

-- | Determines the logging behavior. This option only affects console logs;
-- File logs are unaffected.
data LogMode
  = -- | Sets the logging region to this log.
    LogModeSet
  | -- | Appends the log to the logging region.
    LogModeAppend
  | -- | Closes the logging region, finishing with the log.
    LogModeFinish
  deriving stock (Bounded, Enum, Eq, Show)
