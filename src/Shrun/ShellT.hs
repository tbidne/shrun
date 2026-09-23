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
newtype ShellT env m a = MkShellT (env -> m a)
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
      MonadPathReader,
      MonadPathWriter,
      MonadPosixFiles,
      MonadProcess,
      MonadReader env,
      MonadThread,
      MonadTime,
      MonadThrow
    )
    via (ReaderT env m)
  deriving (MonadTrans) via (ReaderT env)

-- | Runs a 'ShellT' with the given @env@.
runShellT :: forall m env a. ShellT env m a -> env -> m a
runShellT (MkShellT rdr) = rdr
{-# INLINEABLE runShellT #-}

-- Concrete Env here so we can vary our logging logic with other envs
-- (i.e. in tests).

deriving via
  (ReaderT (Env nenv r) m)
  instance
    (MonadRegionLogger m) => MonadRegionLogger (ShellT (Env nenv r) m)

-- REVIEW: Would be nice if we could derive this...

instance (MonadPosixSignals m) => MonadPosixSignals (ShellT env m) where
  raiseSignal = lift . raiseSignal
  {-# INLINEABLE raiseSignal #-}

  signalProcess s = lift . signalProcess s
  {-# INLINEABLE signalProcess #-}

  signalProcessGroup s = lift . signalProcessGroup s
  {-# INLINEABLE signalProcessGroup #-}

  installHandler s h m =
    ask >>= \env ->
      lift $ hFromM <$> installHandler s (hToM env h) m
    where
      hFromM = Signals.mapHandler lift
      hToM env = Signals.mapHandler (`runShellT` env)
  {-# INLINEABLE installHandler #-}

  getSignalMask = lift getSignalMask
  {-# INLINEABLE getSignalMask #-}

  setSignalMask = lift . setSignalMask
  {-# INLINEABLE setSignalMask #-}

  blockSignals = lift . blockSignals
  {-# INLINEABLE blockSignals #-}

  unblockSignals = lift . unblockSignals
  {-# INLINEABLE unblockSignals #-}

  scheduleAlarm = lift . scheduleAlarm
  {-# INLINEABLE scheduleAlarm #-}

  getPendingSignals = lift getPendingSignals
  {-# INLINEABLE getPendingSignals #-}

  awaitSignal = lift . awaitSignal
  {-# INLINEABLE awaitSignal #-}

  setStoppedChildFlag = lift . setStoppedChildFlag
  {-# INLINEABLE setStoppedChildFlag #-}

  queryStoppedChildFlag = lift queryStoppedChildFlag
  {-# INLINEABLE queryStoppedChildFlag #-}
