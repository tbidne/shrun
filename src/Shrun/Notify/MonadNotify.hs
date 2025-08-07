{-# LANGUAGE UndecidableInstances #-}

-- | Provides effects for sending notifications.
module Shrun.Notify.MonadNotify
  ( MonadNotify (..),
    ShrunNote (..),
    NotifyMessage (..),
    fromUnlined,
    NotifyException (..),
    exitFailureToStderr,
  )
where

import DBus.Notify (UrgencyLevel)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Shrun.Configuration.Data.Notify.System
  ( NotifySystemMerged,
    displayNotifySystem,
  )
import Shrun.Configuration.Data.Notify.Timeout (NotifyTimeout)
import Shrun.Data.Text (UnlinedText)
import Shrun.Prelude

newtype NotifyMessage = UnsafeNotifyMessage {unNotifyMessage :: Text}
  deriving stock (Eq, Show)
  deriving newtype (IsString)

instance
  ( k ~ A_Getter,
    a ~ Text,
    b ~ Text
  ) =>
  LabelOptic "unNotifyMessage" k NotifyMessage NotifyMessage a b
  where
  labelOptic = to (\(UnsafeNotifyMessage x) -> x)
  {-# INLINE labelOptic #-}

fromUnlined :: UnlinedText -> NotifyMessage
fromUnlined = UnsafeNotifyMessage . view #unUnlinedText

-- | Holds notification data.
data ShrunNote = MkShrunNote
  { body :: NotifyMessage,
    summary :: NotifyMessage,
    timeout :: NotifyTimeout,
    urgency :: UrgencyLevel
  }
  deriving stock (Eq, Show)

instance
  ( k ~ A_Lens,
    a ~ NotifyMessage,
    b ~ NotifyMessage
  ) =>
  LabelOptic "body" k ShrunNote ShrunNote a b
  where
  labelOptic =
    lensVL
      $ \f (MkShrunNote a1 a2 a3 a4) ->
        fmap
          (\b -> MkShrunNote b a2 a3 a4)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ NotifyMessage,
    b ~ NotifyMessage
  ) =>
  LabelOptic "summary" k ShrunNote ShrunNote a b
  where
  labelOptic =
    lensVL
      $ \f (MkShrunNote a1 a2 a3 a4) ->
        fmap
          (\b -> MkShrunNote a1 b a3 a4)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ NotifyTimeout,
    b ~ NotifyTimeout
  ) =>
  LabelOptic "timeout" k ShrunNote ShrunNote a b
  where
  labelOptic =
    lensVL
      $ \f (MkShrunNote a1 a2 a3 a4) ->
        fmap
          (\b -> MkShrunNote a1 a2 b a4)
          (f a3)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ UrgencyLevel,
    b ~ UrgencyLevel
  ) =>
  LabelOptic "urgency" k ShrunNote ShrunNote a b
  where
  labelOptic =
    lensVL
      $ \f (MkShrunNote a1 a2 a3 a4) ->
        fmap
          (\b -> MkShrunNote a1 a2 a3 b)
          (f a4)
  {-# INLINE labelOptic #-}

-- | Exception for sending desktop notifications.
data NotifyException = MkNotifyException ShrunNote NotifySystemMerged Text
  deriving stock (Eq, Show)

instance Exception NotifyException where
  displayException (MkNotifyException note system message) =
    mconcat
      [ "Exception sending notification with system '",
        displayNotifySystem system,
        "' and note '",
        show note,
        "': ",
        T.unpack message
      ]

-- | General effect for sending notifications.
class (Monad m) => MonadNotify m where
  notify :: (HasCallStack) => ShrunNote -> m (Maybe NotifyException)

-- | Maps (ExitCode, stderr) to Just stderr, if the exit code is
-- ExitFailure.
exitFailureToStderr :: (ExitCode, BSL.ByteString) -> Maybe ByteString
exitFailureToStderr (ex, stderr) = case ex of
  ExitSuccess -> Nothing
  ExitFailure _ -> Just (BSL.toStrict stderr)
