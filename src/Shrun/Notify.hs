{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Module for sending notifications.
--
-- @since X.X
module Shrun.Notify
  ( sendNotif,
  )
where

import DBus.Notify (UrgencyLevel)
import Data.Text qualified as T
import Effects.Exception (catch)
import Shrun.Configuration.Env.Types (HasLogging, HasNotifyConfig (..))
import Shrun.Logging qualified as Logging
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (..))
import Shrun.Logging.Types (Log (..), LogLevel (..), LogMode (..))
import Shrun.Notify.MonadNotify (MonadNotify (..), ShrunNote (..))
import Shrun.Prelude

-- NOTE: This module exists partially so we can isolate unused warnings
-- (i.e. non-linux).

-- | Sends a notification if they are enabled (linux only). Logs any failed
-- sends.
--
-- @since X.X
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
#if OSX
sendNotif _ _ _ = pure ()
#else
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
      Logging.putRegionLog r $
        MkLog
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
#endif
