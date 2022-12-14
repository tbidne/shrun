{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'ShellT' monad transformer.
--
-- @since 0.1
module Shrun.ShellT
  ( ShellT,
    runShellT,
  )
where

import Effects.MonadTime (MonadTime (..))
import Shrun.Configuration.Env.Types (Env)
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (..))
import Shrun.Logging.Types (LogMode (..))
import Shrun.Prelude
import System.Console.Regions qualified as Regions

-- | `ShellT` is the main application type that runs shell commands.
--
-- @since 0.1
type ShellT :: Type -> (Type -> Type) -> Type -> Type
newtype ShellT env m a = MkShellT (ReaderT env m a)
  deriving
    ( -- | @since 0.1
      Functor,
      -- | @since 0.1
      Applicative,
      -- | @since 0.1
      Monad,
      -- | @since 0.1
      MonadReader env,
      -- | @since 0.1
      MonadCatch,
      -- | @since 0.6
      MonadCallStack,
      -- | @since 0.6
      MonadFsReader,
      -- | @since 0.6
      MonadFsWriter,
      -- | @since 0.1
      MonadIO,
      -- | @since 0.6
      MonadIORef,
      -- | @since 0.1
      MonadMask,
      -- | @since 0.6
      MonadTBQueue,
      -- | @since 0.6
      MonadTerminal,
      -- | @since 0.6
      MonadThread,
      -- | @since 0.5
      MonadTime,
      -- | @since 0.1
      MonadThrow,
      -- | @since 0.6
      MonadTVar,
      -- | @since 0.1
      MonadUnliftIO
    )
    via (ReaderT env m)

-- | Runs a 'ShellT' with the given @env@.
--
-- @since 0.1
runShellT :: ShellT env m a -> env -> m a
runShellT (MkShellT rdr) = runReaderT rdr
{-# INLINEABLE runShellT #-}

-- Concrete Env here so we can vary our logging logic with other envs
-- (i.e. in tests).

-- | @since 0.1
instance
  (MonadIO m, MonadMask m, MonadTerminal m) =>
  MonadRegionLogger (ShellT Env m)
  where
  type Region (ShellT Env m) = ConsoleRegion

  logGlobal = putTextLn

  logRegion LogModeSet cr = liftIO . Regions.setConsoleRegion cr
  logRegion LogModeAppend cr = liftIO . Regions.appendConsoleRegion cr
  logRegion LogModeFinish cr = liftIO . Regions.finishConsoleRegion cr

  withRegion = Regions.withConsoleRegion

  displayRegions = Regions.displayConsoleRegions
