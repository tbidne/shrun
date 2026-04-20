{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'ShellT' monad transformer.
module Shrun.ShellT
  ( ShellT,
    runShellT,
  )
where

import Effects.System.Posix.Signals
  ( MonadPosixSignals
      ( awaitSignal,
        blockSignals,
        getPendingSignals,
        getSignalMask,
        installHandler,
        queryStoppedChildFlag,
        raiseSignal,
        scheduleAlarm,
        setSignalMask,
        setStoppedChildFlag,
        signalProcess,
        signalProcessGroup,
        unblockSignals
      ),
  )
import Effects.System.Posix.Signals qualified as Signals
import Shrun.Configuration.Env.Types (Env)
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger)
import Shrun.Prelude

-- | `ShellT` is the main application type that runs shell commands.
type ShellT :: Type -> (Type -> Type) -> Type -> Type
newtype ShellT env m a = MkShellT (ReaderT env m a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadAsync,
      MonadAtomic,
      MonadCatch,
      MonadEvaluate,
      MonadFileReader,
      MonadFileWriter,
      MonadHandleReader,
      MonadHandleWriter,
      MonadIO,
      MonadIORef,
      MonadMask,
      MonadMVar,
      MonadNotify,
      MonadPathWriter,
      MonadPosixFiles,
      MonadProcess,
      MonadReader env,
      MonadThread,
      MonadTime,
      MonadThrow
    )
    via (ReaderT env m)

unShellT :: ShellT env m a -> ReaderT env m a
unShellT (MkShellT rdr) = rdr

-- | Runs a 'ShellT' with the given @env@.
runShellT :: forall m env a. ShellT env m a -> env -> m a
runShellT (MkShellT rdr) = runReaderT rdr
{-# INLINEABLE runShellT #-}

-- Concrete Env here so we can vary our logging logic with other envs
-- (i.e. in tests).

-- Can't use @deriving via m@ due to a bug: GHC version 9.2.5: No skolem info:@.
-- https://gitlab.haskell.org/ghc/ghc/-/issues/15376

deriving newtype instance (MonadRegionLogger m) => MonadRegionLogger (ShellT (Env r) m)

-- REVIEW: Would be nice if we could derive this...

instance (MonadPosixSignals m) => MonadPosixSignals (ShellT env m) where
  raiseSignal = MkShellT . raiseSignal
  {-# INLINEABLE raiseSignal #-}

  signalProcess s = MkShellT . signalProcess s
  {-# INLINEABLE signalProcess #-}

  signalProcessGroup s = MkShellT . signalProcessGroup s
  {-# INLINEABLE signalProcessGroup #-}

  installHandler s h m = MkShellT $ do
    hFromM <$> installHandler s (hToM h) m
    where
      hFromM = Signals.mapHandler MkShellT
      hToM = Signals.mapHandler unShellT
  {-# INLINEABLE installHandler #-}

  getSignalMask = MkShellT getSignalMask
  {-# INLINEABLE getSignalMask #-}

  setSignalMask = MkShellT . setSignalMask
  {-# INLINEABLE setSignalMask #-}

  blockSignals = MkShellT . blockSignals
  {-# INLINEABLE blockSignals #-}

  unblockSignals = MkShellT . unblockSignals
  {-# INLINEABLE unblockSignals #-}

  scheduleAlarm = MkShellT . scheduleAlarm
  {-# INLINEABLE scheduleAlarm #-}

  getPendingSignals = MkShellT getPendingSignals
  {-# INLINEABLE getPendingSignals #-}

  awaitSignal = MkShellT . awaitSignal
  {-# INLINEABLE awaitSignal #-}

  setStoppedChildFlag = MkShellT . setStoppedChildFlag
  {-# INLINEABLE setStoppedChildFlag #-}

  queryStoppedChildFlag = MkShellT queryStoppedChildFlag
  {-# INLINEABLE queryStoppedChildFlag #-}
