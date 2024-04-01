-- | Module for sending notifications.
module Shrun.Notify
  ( sendNotif,
  )
where

import DBus.Notify (UrgencyLevel)
import Data.Text qualified as T
import Shrun.Env.Types
  ( HasAnyError,
    HasLogging,
    HasNotifyConfig (getNotifyConfig),
    setAnyErrorTrue,
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
  ( HasAnyError env,
    HasLogging env (Region m),
    HasNotifyConfig env,
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
      notify (mkNote timeout) >>= \case
        Nothing -> pure ()
        Just notifyEx -> withRegion Linear (logEx notifyEx)

    logEx ex r = do
      -- set exit code
      setAnyErrorTrue
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
