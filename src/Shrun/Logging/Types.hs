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
    defaultCmdLogReadSize,
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

instance
  ( k ~ A_Lens,
    a ~ Maybe CommandP1,
    b ~ Maybe CommandP1
  ) =>
  LabelOptic "cmd" k Log Log a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkLog _cmd _msg _lvl _mode) ->
          fmap
            (\cmd' -> MkLog cmd' _msg _lvl _mode)
            (f _cmd)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ Text,
    b ~ Text
  ) =>
  LabelOptic "msg" k Log Log a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkLog _cmd _msg _lvl _mode) ->
          fmap
            (\msg' -> MkLog _cmd msg' _lvl _mode)
            (f _msg)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ LogLevel,
    b ~ LogLevel
  ) =>
  LabelOptic "lvl" k Log Log a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkLog _cmd _msg _lvl _mode) ->
          fmap
            (\lvl' -> MkLog _cmd _msg lvl' _mode)
            (f _lvl)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ LogMode,
    b ~ LogMode
  ) =>
  LabelOptic "mode" k Log Log a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkLog _cmd _msg _lvl _mode) ->
          fmap
            (MkLog _cmd _msg _lvl)
            (f _mode)
  {-# INLINE labelOptic #-}

defaultCmdLogReadSize :: Bytes B Natural
defaultCmdLogReadSize = MkBytes 1024
