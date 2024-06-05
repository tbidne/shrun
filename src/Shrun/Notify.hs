-- | Module for sending notifications.
module Shrun.Notify
  ( sendNotif,
  )
where

import DBus.Notify (UrgencyLevel)
import Shrun.Configuration.Env.Types
  ( HasAnyError,
    HasCommonLogging,
    HasConsoleLogging,
    HasFileLogging,
    HasNotifyConfig (getNotifyConfig),
    setAnyErrorTrue,
  )
import Shrun.Data.Text (UnlinedText)
import Shrun.Data.Text qualified as ShrunText
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
    HasCallStack,
    HasCommonLogging env,
    HasConsoleLogging env (Region m),
    HasFileLogging env,
    HasNotifyConfig env,
    MonadNotify m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
    MonadTime m
  ) =>
  -- | Notif summary
  UnlinedText ->
  -- | Notif body
  UnlinedText ->
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
    {-# INLINEABLE notifyWithErrorLogging #-}

    logEx ex r = do
      -- set exit code
      setAnyErrorTrue
      Logging.putRegionLog r
        $ MkLog
          { cmd = Nothing,
            msg =
              "Could not send notification: "
                <> ShrunText.fromTextReplace (pack (displayException ex)),
            lvl = LevelError,
            mode = LogModeFinish
          }
    {-# INLINEABLE logEx #-}

    mkNote timeout =
      MkShrunNote
        { summary,
          body,
          urgency,
          timeout
        }
    {-# INLINEABLE mkNote #-}
