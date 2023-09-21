-- | Module for sending notifications.
module Shrun.Notify
  ( sendNotif,
  )
where

import DBus.Notify (UrgencyLevel)
import Data.Text qualified as T
import Effects.Exception (catch)
import Shrun.Configuration.Env.Types
  ( HasLogging,
    HasNotifyConfig (getNotifyConfig),
  )
import Shrun.Logging qualified as Logging
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (Region, withRegion))
import Shrun.Logging.Types
  ( Log (MkLog, cmd, lvl, mode, msg),
    LogLevel (LevelError),
    LogMode (LogModeFinish),
  )
import Shrun.Notify.MonadNotify
  ( MonadNotify (notify),
    ShrunNote (MkShrunNote, body, summary, timeout, urgency),
  )
import Shrun.Prelude

-- | Sends a notification if they are enabled (linux only). Logs any failed
-- sends.
sendNotif ::
  ( HasLogging env (Region m),
    HasNotifyConfig env,
    MonadCatch m,
    MonadNotify m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
    MonadTime m
  ) =>
  -- | Notif summary
  Text ->
  -- | Notif body
  Text ->
  -- | Notif urgency
  UrgencyLevel ->
  m ()
sendNotif summary body urgency = do
  cfg <- asks getNotifyConfig
  traverse_ notifyWithErrorLogging (cfg ^? (_Just % #timeout))
  where
    notifyWithErrorLogging timeout =
      notify (mkNote timeout)
        `catch` \((MkExceptionCS someEx _) :: ExceptionCS SomeException) ->
          withRegion Linear (logEx someEx)
            `catchAny` \ex -> withRegion Linear (logEx ex)

    logEx ex r =
      Logging.putRegionLog r
        $ MkLog
          { cmd = Nothing,
            msg = "Could not send notification: " <> T.pack (displayException ex),
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
