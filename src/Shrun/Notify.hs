-- | Module for sending notifications.
module Shrun.Notify
  ( sendNotif,
    formatNotifyMessage,
  )
where

import DBus.Notify (UrgencyLevel)
import Data.Text qualified as T
import Shrun.Configuration.Env.Types
  ( HasAnyError,
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
import Shrun.Notify.MonadNotify
  ( MonadNotify (notify),
    NotifyMessage (UnsafeNotifyMessage),
    ShrunNote (MkShrunNote, body, summary, timeout, urgency),
  )
import Shrun.Prelude
import Shrun.Utils qualified as U

-- | Sends a notification if they are With (linux only). Logs any failed
-- sends.
sendNotif ::
  ( HasAnyError env,
    HasCallStack,
    HasLogging env m,
    HasNotifyConfig env,
    MonadNotify m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
    MonadTime m
  ) =>
  -- | Notif summary
  NotifyMessage ->
  -- | Notif body
  NotifyMessage ->
  -- | Notif urgency
  UrgencyLevel ->
  m ()
sendNotif summary body urgency = do
  cfg <- asks getNotifyConfig
  traverse_ notifyWithErrorLogging (cfg ^? (_Just % #timeout))
  where
    notifyWithErrorLogging timeout =
      notify (mkNote timeout) >>= \case
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
      MkShrunNote
        { summary,
          body,
          urgency,
          timeout
        }

formatNotifyMessage :: UnlinedText -> [UnlinedText] -> NotifyMessage
formatNotifyMessage timeTxt messages =
  UnsafeNotifyMessage
    . T.intercalate "\n"
    . fmap (U.stripControlAll . view #unUnlinedText)
    $ timeTxt
    : messages
