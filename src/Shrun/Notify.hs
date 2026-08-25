{-# LANGUAGE TemplateHaskell #-}
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
    LogLevel (LevelError, LevelWarn),
    LogMode (LogModeFinish),
  )
import Shrun.Logging.Types qualified as Types
import Shrun.Prelude
import Shrun.Utils qualified as U

newtype NotifyMessage = UnsafeNotifyMessage {unNotifyMessage :: Text}
  deriving stock (Eq, Show)
  deriving newtype (IsString)

makeFieldLabelsNoPrefixReadOnly ''NotifyMessage

fromUnlined :: UnlinedText -> NotifyMessage
fromUnlined = UnsafeNotifyMessage . view #unUnlinedText

-- | Sends a notification if they are With (linux only). Logs any failed
-- sends.
sendNotif ::
  forall m env notifyEnv.
  ( HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasLogging env m,
    HasNotifyConfig env notifyEnv,
    MonadAtomic m,
    MonadCatch m,
    MonadNotify m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadTime m,
    NotifyEnvF m ~ notifyEnv
  ) =>
  -- | Notif summary
  NotifyMessage ->
  -- | Notif body
  NotifyMessage ->
  -- | Notif urgency
  NotifyUrgency ->
  m ()
sendNotif summary body urgency = do
  asks (getNotifyConfig @env @notifyEnv) >>= \case
    Nothing -> pure ()
    Just notifyConfig ->
      notifyWithErrorLogging
        (notifyConfig ^. #system)
        (notifyConfig ^. #timeout)
  where
    notifyWithErrorLogging notifyEnv timeout =
      try @_ @Notify.NotifyException (Notify.notify notifyEnv (mkNote timeout)) >>= \case
        Right () -> pure ()
        Left notifyEx -> do
          let exMsg = displayExceptiont (notifyEx ^. #exception)
              isTooMany = "Created too many similar notifications in quick succession" `T.isInfixOf` exMsg

          -- Rethrow fatal exceptions, except for "too many" ones. This is
          -- triggered by DBus when we run identical commands e.g.
          --
          --   shrun "sleep 1" "sleep 1"
          --
          -- But it is not important, so ignore it.
          if
            | notifyEx ^. #fatal && not isTooMany -> throwM notifyEx
            | notifyEx ^. #fatal && isTooMany -> withRegion Linear logTooMany
            | otherwise -> withRegion Linear (logEx notifyEx)

    logTooMany r =
      Logging.putRegionLog r
        $ MkLog
          { cmd = Nothing,
            msg =
              Types.fromUnlined
                "Could not send notification: sent too many similar notifications.",
            lvl = LevelWarn,
            mode = LogModeFinish
          }

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
