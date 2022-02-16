{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'NoLegendMockShell' type.
module MockShell.NoLegendMockShell (NoLegendMockShell (..)) where

import MockEnv (MockEnv)
import MockShell.MockShellBase (MockShellBase (..))
import ShellRun.Class.MonadShell (MonadShell (..))
import ShellRun.Command (Command (..))
import ShellRun.Data.NonEmptySeq qualified as NESeq
import ShellRun.Legend (LegendErr (..))
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import ShellRun.Prelude

-- | 'NoLegendMockShell' is intended to test a run of
-- 'ShellRun.runShell' when the legend is not included.
type NoLegendMockShell :: Type -> Type
newtype NoLegendMockShell a = MkNoLegendMockShell
  {runNoLegendMockShell :: MockShellBase a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader MockEnv,
      MonadWriter (List Text),
      RegionLogger
    )
    via MockShellBase

instance MonadShell NoLegendMockShell where
  -- Purposely giving a bad shell function here to prove that no legend skips
  -- this (otherwise would die here)
  legendPathToMap _ = pure $ Left $ EntryErr "Bad key"
  runCommands = tell . NESeq.toList . fmap command

instance Show a => Show (NoLegendMockShell a) where
  show x = "MkNoLegendMockShell " <> show x
