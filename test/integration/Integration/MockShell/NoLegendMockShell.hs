{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'NoLegendMockShell' type.
module Integration.MockShell.NoLegendMockShell
  ( NoLegendMockShell (..),
    runNoLegendMockShell,
  )
where

import Integration.IntEnv (IntEnv)
import Integration.Prelude
import ShellRun.Effects.MonadFSReader (MonadFSReader (..))
import ShellRun.Effects.MonadProcRunner (MonadProcRunner (..))
import ShellRun.Effects.MonadTime (MonadTime (..))
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import ShellRun.ShellT (ShellT, runShellT)

-- | 'NoLegendMockShell' is intended to test a run of
-- 'ShellRun.runShell' when the legend is not included.
type NoLegendMockShell :: Type -> Type
newtype NoLegendMockShell a = MkNoLegendMockShell (ShellT IntEnv IO a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadCatch,
      MonadIO,
      MonadMask,
      MonadProcRunner,
      MonadReader IntEnv,
      MonadThrow,
      MonadTime,
      MonadUnliftIO,
      RegionLogger
    )
    via ShellT IntEnv IO

runNoLegendMockShell :: NoLegendMockShell a -> IntEnv -> IO a
runNoLegendMockShell (MkNoLegendMockShell rdr) = runShellT rdr

instance MonadFSReader NoLegendMockShell where
  getXdgConfig _ = pure "config"
  readFile "config/shell-run.legend" = throwString "FileNotFound"
  readFile _ = pure "Bad key"

instance Show a => Show (NoLegendMockShell a) where
  show x = "MkNoLegendMockShell " <> show x
