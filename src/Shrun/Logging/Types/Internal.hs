{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal module for logging types.
--
-- @since 0.1
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
--
-- @since 0.7
newtype FileLog = UnsafeFileLog
  { -- | @since 0.7
    unFileLog :: Text
  }
  deriving stock
    ( -- | @since 0.7
      Eq,
      -- | @since 0.7
      Show
    )

-- | @since 0.7
makeFieldLabelsWith
  (noPrefixFieldLabels & generateUpdateableOptics .~ False)
  ''FileLog

-- | 'ConsoleLog' is a textual representation of a given log after it has
-- been formatted.
--
-- @since 0.7
newtype ConsoleLog = UnsafeConsoleLog
  { -- | @since 0.7
    unConsoleLog :: Text
  }
  deriving stock
    ( -- | @since 0.7
      Eq,
      -- | @since 0.7
      Show
    )

-- | @since 0.7
makeFieldLabelsWith
  (noPrefixFieldLabels & generateUpdateableOptics .~ False)
  ''ConsoleLog

-- NOTE: LogMode exists here so we do not have cyclic dependencies w/
-- RegionLogger

-- | Determines the logging behavior. This option only affects console logs;
-- File logs are unaffected.
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
