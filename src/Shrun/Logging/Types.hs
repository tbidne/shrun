{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the `Log` type and associated functions.
module Shrun.Logging.Types
  ( -- * Basic Types
    Log (..),
    LogMessage (..),
    fromUnlined,
    unsafeMapLogMessage,
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
import Shrun.Data.Text (UnlinedText)
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

newtype LogMessage = UnsafeLogMessage Text
  deriving stock (Eq, Show)
  deriving newtype (IsString)

instance
  ( k ~ A_Getter,
    a ~ Text,
    b ~ Text
  ) =>
  LabelOptic "unLogMessage" k LogMessage LogMessage a b
  where
  labelOptic = to (\(UnsafeLogMessage t) -> t)
  {-# INLINE labelOptic #-}

fromUnlined :: UnlinedText -> LogMessage
fromUnlined = UnsafeLogMessage . view #unUnlinedText

unsafeMapLogMessage :: (Text -> Text) -> LogMessage -> LogMessage
unsafeMapLogMessage f (UnsafeLogMessage m) = UnsafeLogMessage (f m)

-- | Captures the relevant information concerning a specific log
-- (i.e. command, text, level, and mode).
data Log = MkLog
  { -- | Optional command that produced this log.
    cmd :: Maybe CommandP1,
    -- | The 'Text' for a given log.
    msg :: LogMessage,
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
      $ \f (MkLog a1 a2 a3 a4) ->
        fmap
          (\b -> MkLog b a2 a3 a4)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ LogMessage,
    b ~ LogMessage
  ) =>
  LabelOptic "msg" k Log Log a b
  where
  labelOptic =
    lensVL
      $ \f (MkLog a1 a2 a3 a4) ->
        fmap
          (\b -> MkLog a1 b a3 a4)
          (f a2)
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
      $ \f (MkLog a1 a2 a3 a4) ->
        fmap
          (\b -> MkLog a1 a2 b a4)
          (f a3)
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
      $ \f (MkLog a1 a2 a3 a4) ->
        fmap
          (\b -> MkLog a1 a2 a3 b)
          (f a4)
  {-# INLINE labelOptic #-}
