{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'ShellT' monad transformer.
module Shrun.ShellT
  ( ShellT,
    runShellT,
  )
where

import DBus.Notify (Hint (Urgency), Note (..), UrgencyLevel (..))
import DBus.Notify qualified as DBusN
import Data.Text qualified as T
import Shrun.Configuration.Env.Types (Env)
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (..))
import Shrun.Notify.MonadDBus (MonadDBus)
import Shrun.Notify.MonadDBus qualified as MonadDBus
import Shrun.Notify.MonadNotify (MonadNotify (..), ShrunNote)
import Shrun.Notify.MonadNotifySend (MonadNotifySend)
import Shrun.Notify.MonadNotifySend qualified as MonadNotifySend
import Shrun.Notify.Types (NotifySystem (..), NotifyTimeout (..))
import Shrun.Prelude

-- | `ShellT` is the main application type that runs shell commands.
type ShellT :: Type -> (Type -> Type) -> Type -> Type
newtype ShellT env m a = MkShellT (ReaderT env m a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadAsync,
      MonadDBus,
      MonadCatch,
      MonadFileReader,
      MonadFileWriter,
      MonadHandleReader,
      MonadHandleWriter,
      MonadIO,
      MonadIORef,
      MonadMask,
      MonadNotifySend,
      MonadPathWriter,
      MonadProcess,
      MonadReader env,
      MonadSTM,
      MonadTerminal,
      MonadThread,
      MonadTime,
      MonadThrow
    )
    via (ReaderT env m)

-- | Runs a 'ShellT' with the given @env@.
runShellT :: ShellT env m a -> env -> m a
runShellT (MkShellT rdr) = runReaderT rdr
{-# INLINEABLE runShellT #-}

-- Concrete Env here so we can vary our logging logic with other envs
-- (i.e. in tests).

-- Can't use @deriving via m@ due to a bug: GHC version 9.2.5: No skolem info:@.
-- https://gitlab.haskell.org/ghc/ghc/-/issues/15376

deriving newtype instance (MonadRegionLogger m) => MonadRegionLogger (ShellT Env m)

instance (MonadDBus m, MonadNotifySend m) => MonadNotify (ShellT Env m) where
  notify note = asks (preview (#notifyEnv %? #system)) >>= traverse_ sendNote
    where
      sendNote (DBus client) = void $ MonadDBus.notify client (shrunToDBus note)
      sendNote NotifySend = MonadNotifySend.notify (shrunToNotifySend note)

shrunToDBus :: ShrunNote -> Note
shrunToDBus shrunNote =
  DBusN.Note
    { appName = "Shrun",
      summary = unpack $ shrunNote ^. #summary,
      body = Just . DBusN.Text . T.unpack $ shrunNote ^. #body,
      appImage = Nothing,
      hints = [Urgency (shrunNote ^. #urgency)],
      expiry,
      actions = []
    }
  where
    expiry = case shrunNote ^. #timeout of
      NotifyTimeoutNever -> DBusN.Never
      NotifyTimeoutSeconds s ->
        DBusN.Milliseconds $ 1_000 * fromIntegral s

shrunToNotifySend :: ShrunNote -> Text
shrunToNotifySend shrunNote = txt
  where
    txt =
      mconcat
        [ "notify-send ",
          " --app-name Shrun \"",
          shrunNote ^. #summary,
          "\" ",
          (\b -> " \"" <> b <> "\" ") (shrunNote ^. #body),
          ulToNS (shrunNote ^. #urgency),
          timeout
        ]

    ulToNS Low = " --urgency low "
    ulToNS Normal = " --urgency normal "
    ulToNS Critical = " --urgency critical "

    timeout = case shrunNote ^. #timeout of
      NotifyTimeoutNever -> " --expire-time 0 "
      NotifyTimeoutSeconds s ->
        mconcat
          [ " --expire-time ",
            showt (fromIntegral @_ @Integer s * 1_000)
          ]
