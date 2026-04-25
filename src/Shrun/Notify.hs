{-# LANGUAGE UndecidableInstances #-}

-- | Module for sending notifications.
module Shrun.Notify
  ( NotifyMessage (..),
    fromUnlined,
    sendNotif,
    formatNotifyMessage,
  )
where

import Data.Text qualified as T
import Effects.Notify qualified as Notify
import Shrun.Configuration.Env.Types
  ( HasAnyError,
    HasCommands,
    HasLogging,
    HasNotifyConfig (getNotifyConfig),
    setAnyErrorTrue,
  )
import Shrun.Data.Text (UnlinedText)
import Shrun.Data.Text qualified as ShrunText
import Shrun.Logging qualified as Logging
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (withRegion))
import Shrun.Logging.Types
  ( Log (MkLog, cmd, lvl, mode, msg),
    LogLevel (LevelError),
    LogMode (LogModeFinish),
  )
import Shrun.Logging.Types qualified as Types
import Shrun.Prelude
import Shrun.Utils qualified as U

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

-- | Sends a notification if they are With (linux only). Logs any failed
-- sends.
sendNotif ::
  ( HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasLogging env m,
    HasNotifyConfig env,
    MonadAtomic m,
    MonadCatch m,
    MonadNotify m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadTime m
  ) =>
  -- | Notif summary
  NotifyMessage ->
  -- | Notif body
  NotifyMessage ->
  -- | Notif urgency
  NotifyUrgency ->
  m ()
sendNotif summary body urgency = do
  asks getNotifyConfig >>= \case
    Nothing -> pure ()
    Just notifyConfig ->
      notifyWithErrorLogging
        (notifyConfig ^. #system)
        (notifyConfig ^. #timeout)
  where
    notifyWithErrorLogging notifyEnv timeout =
      Notify.tryNonFatalNotify notifyEnv (mkNote timeout) >>= \case
        Nothing -> pure ()
        Just notifyEx -> withRegion Linear (logEx notifyEx)

    logEx ex r = do
      -- set exit code
      setAnyErrorTrue
      Logging.putRegionLog r
        $ MkLog
          { cmd = Nothing,
            msg =
              Types.fromUnlined
                $ "Could not send notification: "
                <> ShrunText.fromTextReplace (pack (displayException ex)),
            lvl = LevelError,
            mode = LogModeFinish
          }

    mkNote timeout =
      Notify.mkNote (summary ^. #unNotifyMessage)
        & Notify.setBody (Just $ body ^. #unNotifyMessage)
        & Notify.setTimeout (Just timeout)
        & Notify.setTitle (Just "Shrun")
        & Notify.setUrgency (Just urgency)

formatNotifyMessage :: UnlinedText -> [UnlinedText] -> NotifyMessage
formatNotifyMessage timeTxt messages =
  UnsafeNotifyMessage
    . T.intercalate "\n"
    . fmap (U.stripControlAll . view #unUnlinedText)
    $ timeTxt
    : messages
