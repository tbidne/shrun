{-# LANGUAGE UndecidableInstances #-}

-- | Provides effects for sending notifications.
module Shrun.Notify.MonadNotify
  ( MonadNotify (..),
    ShrunNote (..),
    NotifyException (..),
    exitFailureToStderr,
  )
where

import DBus.Notify (UrgencyLevel)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Shrun.Data.Text (UnlinedText)
import Shrun.Notify.Types
  ( NotifySystemMerged,
    NotifyTimeout,
    displayNotifySystem,
  )
import Shrun.Prelude

-- | Holds notification data.
data ShrunNote = MkShrunNote
  { summary :: UnlinedText,
    body :: UnlinedText,
    urgency :: UrgencyLevel,
    timeout :: NotifyTimeout
  }
  deriving stock (Eq, Show)

instance
  ( k ~ A_Lens,
    a ~ UnlinedText,
    b ~ UnlinedText
  ) =>
  LabelOptic "summary" k ShrunNote ShrunNote a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkShrunNote _summary _body _urgency _timeout) ->
          fmap
            (\summary' -> MkShrunNote summary' _body _urgency _timeout)
            (f _summary)
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
      $ \f
         (MkShrunNote _summary _body _urgency _timeout) ->
          fmap
            (\urgency' -> MkShrunNote _summary _body urgency' _timeout)
            (f _urgency)
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
      $ \f
         (MkShrunNote _summary _body _urgency _timeout) ->
          fmap
            (MkShrunNote _summary _body _urgency)
            (f _timeout)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ UnlinedText,
    b ~ UnlinedText
  ) =>
  LabelOptic "body" k ShrunNote ShrunNote a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkShrunNote _summary _body _urgency _timeout) ->
          fmap
            (\body' -> MkShrunNote _summary body' _urgency _timeout)
            (f _body)
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
