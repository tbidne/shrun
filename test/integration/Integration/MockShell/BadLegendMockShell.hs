{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'GoodMockShell' type.
module Integration.MockShell.BadLegendMockShell (BadLegendMockShell (..)) where

import Integration.MockEnv (MockEnv)
import Integration.MockShell.MockShellBase (MockShellBase (..))
import Integration.Prelude
import ShellRun.Class.MonadShell (MonadShell (..))
import ShellRun.Legend (LegendErr (..))
import ShellRun.Logging.RegionLogger (RegionLogger (..))

-- | 'BadLegendMockShell' is intended to test a run of
-- 'ShellRun.runShell' when the path to the legend file is bad.
type BadLegendMockShell :: Type -> Type
newtype BadLegendMockShell a = MkBadLegendMockShell
  {runBadLegendMockShell :: MockShellBase a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader MockEnv,
      MonadWriter (List Text),
      RegionLogger
    )
    via MockShellBase

instance MonadShell BadLegendMockShell where
  getDefaultDir = pure "config"
  legendPathToMap _ = pure $ Left $ FileErr "File not found"
  runCommands _ = pure ()

instance Show a => Show (BadLegendMockShell a) where
  show _ = "MkBadLegendMockShell"
