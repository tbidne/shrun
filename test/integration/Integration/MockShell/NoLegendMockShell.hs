{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'NoLegendMockShell' type.
module Integration.MockShell.NoLegendMockShell
  ( NoLegendMockShell (..),
    runNoLegendMockShell,
  )
where

import Integration.MockEnv (MockEnv)
import Integration.MockShell.MockShellBase (MockShellBase, runMockShellBase)
import Integration.Prelude
import ShellRun.Effects.MonadFSReader (MonadFSReader (..))
import ShellRun.Logging.RegionLogger (RegionLogger (..))

-- | 'NoLegendMockShell' is intended to test a run of
-- 'ShellRun.runShell' when the legend is not included.
type NoLegendMockShell :: Type -> Type
newtype NoLegendMockShell a = MkNoLegendMockShell (MockShellBase a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadCatch,
      MonadIO,
      MonadMask,
      MonadReader MockEnv,
      MonadThrow,
      MonadUnliftIO,
      RegionLogger
    )
    via MockShellBase

runNoLegendMockShell :: NoLegendMockShell a -> MockEnv -> IO a
runNoLegendMockShell (MkNoLegendMockShell rdr) = runMockShellBase rdr

instance MonadFSReader NoLegendMockShell where
  getXdgConfig _ = pure "config"
  readFile "config/shell-run.legend" = throwString "FileNotFound"
  readFile _ = pure "Bad key"

instance Show a => Show (NoLegendMockShell a) where
  show x = "MkNoLegendMockShell " <> show x
