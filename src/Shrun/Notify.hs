{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Module for sending notifications.
module Shrun.Notify
  ( sendNotif,
    runNotifyDynamic,
  )
where

import DBus.Notify (UrgencyLevel)
import Data.Text qualified as T
import Effectful.Dispatch.Dynamic (interpret)
import Shrun.Configuration.Env.Types (Env, HasLogging, HasNotifyConfig (getNotifyConfig))
import Shrun.Logging qualified as Logging
import Shrun.Logging.RegionLogger (RegionLoggerDynamic)
import Shrun.Logging.RegionLogger qualified as RegionLogger
import Shrun.Logging.Types
  ( Log (MkLog, cmd, lvl, mode, msg),
    LogLevel (LevelError),
    LogMode (LogModeFinish),
  )
import Shrun.Notify.AppleScript (AppleScriptDynamic)
import Shrun.Notify.AppleScript qualified as AppleScript
import Shrun.Notify.DBus (DBusDynamic)
import Shrun.Notify.DBus qualified as DBus
import Shrun.Notify.Notify
  ( NotifyDynamic (Notify),
    ShrunNote (MkShrunNote, body, summary, timeout, urgency),
  )
import Shrun.Notify.Notify qualified as Notify
import Shrun.Notify.NotifySend (NotifySendDynamic)
import Shrun.Notify.NotifySend qualified as NotifySend
import Shrun.Notify.Types (NotifySystem (AppleScript, DBus, NotifySend))
import Shrun.Prelude

-- | Sends a notification if they are enabled (linux only). Logs any failed
-- sends.
sendNotif ::
  forall env r es.
  ( Concurrent :> es,
    HasLogging env r,
    HasNotifyConfig env,
    NotifyDynamic :> es,
    Reader env :> es,
    RegionLoggerDynamic r :> es,
    TimeDynamic :> es
  ) =>
  -- | Notif summary
  Text ->
  -- | Notif body
  Text ->
  -- | Notif urgency
  UrgencyLevel ->
  Eff es ()
sendNotif summary body urgency = do
  cfg <- asks @env getNotifyConfig
  traverse_ notifyWithErrorLogging (cfg ^? (_Just % #timeout))
  where
    notifyWithErrorLogging timeout =
      Notify.notify (mkNote timeout)
        `catch` \(someEx :: SomeException) ->
          RegionLogger.withRegion @r Linear (logEx someEx)
            `catchAny` \ex -> RegionLogger.withRegion @r Linear (logEx ex)

    logEx :: (Exception e) => e -> r -> Eff es ()
    logEx ex r =
      Logging.putRegionLog @env @r r
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

runNotifyDynamic ::
  ( AppleScriptDynamic :> es,
    DBusDynamic :> es,
    NotifySendDynamic :> es,
    Reader Env :> es
  ) =>
  Eff (NotifyDynamic : es) a ->
  Eff es a
runNotifyDynamic = interpret $ \_ -> \case
  Notify note -> asks @Env (preview (#notifyEnv %? #system)) >>= traverse_ sendNote
    where
      sendNote (DBus client) = DBus.notifyDBus client note
      sendNote NotifySend = NotifySend.notifyNotifySend note
      sendNote AppleScript = AppleScript.notifyAppleScript note
