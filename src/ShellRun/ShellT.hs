{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'ShellT' monad transformer.
--
-- @since 0.1
module ShellRun.ShellT
  ( ShellT,
    runShellT,
  )
where

import ShellRun.Effects.MonadFSReader (MonadFSReader (..))
import ShellRun.Effects.MonadTime (MonadTime (..))
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import ShellRun.Logging.Types (LogMode (..))
import ShellRun.Prelude
import System.Console.Regions (ConsoleRegion)
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
      -- | @since 0.1
      MonadFSReader,
      -- | @since 0.1
      MonadIO,
      -- | @since 0.1
      MonadMask,
      -- | @since 0.1
      MonadTime,
      -- | @since 0.1
      MonadThrow,
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

-- | @since 0.1
instance (MonadIO m) => RegionLogger (ShellT env m) where
  type Region (ShellT env m) = ConsoleRegion

  logFn = liftIO . putStrLn

  logModeToRegionFn Set cr = liftIO . Regions.setConsoleRegion cr
  logModeToRegionFn Append cr = liftIO . Regions.appendConsoleRegion cr
  logModeToRegionFn Finish cr = liftIO . Regions.finishConsoleRegion cr
