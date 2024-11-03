{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'ShellT' monad transformer.
module Shrun.ShellT
  ( ShellT,
    runShellT,
  )
where

import Shrun.Configuration.Data.Notify.System
  ( NotifySystemP (AppleScript, DBus, NotifySend),
  )
import Shrun.Configuration.Env.Types (Env)
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger)
import Shrun.Notify.AppleScript qualified as AppleScript
import Shrun.Notify.DBus (MonadDBus)
import Shrun.Notify.DBus qualified as DBus
import Shrun.Notify.MonadNotify (MonadNotify (notify))
import Shrun.Notify.NotifySend qualified as NotifySend
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
      MonadPathWriter,
      MonadTypedProcess,
      MonadReader env,
      MonadSTM,
      MonadThread,
      MonadTime,
      MonadThrow
    )
    via (ReaderT env m)

-- | Runs a 'ShellT' with the given @env@.
runShellT :: forall m env a. ShellT env m a -> env -> m a
runShellT (MkShellT rdr) = runReaderT rdr
{-# INLINEABLE runShellT #-}

-- Concrete Env here so we can vary our logging logic with other envs
-- (i.e. in tests).

-- Can't use @deriving via m@ due to a bug: GHC version 9.2.5: No skolem info:@.
-- https://gitlab.haskell.org/ghc/ghc/-/issues/15376

deriving newtype instance (MonadRegionLogger m) => MonadRegionLogger (ShellT (Env r) m)

instance
  ( MonadDBus m,
    MonadTypedProcess m
  ) =>
  MonadNotify (ShellT (Env r) m)
  where
  notify note =
    asks (preview (#config % #notify %? #system)) >>= \case
      Nothing -> pure Nothing
      Just nenv -> sendNote nenv
    where
      sendNote (DBus client) = DBus.notifyDBus client note
      sendNote NotifySend = NotifySend.notifyNotifySend note
      sendNote AppleScript = AppleScript.notifyAppleScript note
  {-# INLINEABLE notify #-}
