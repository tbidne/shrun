-- | Provides the 'NoLegendMockShell' type.
module MockShell.NoLegendMockShell (NoLegendMockShell (..)) where

import Control.Monad.Writer qualified as MTL
import MockEnv (MockEnv)
import MockShell.MockShellBase (MockShellBase (..))
import ShellRun.Class.MonadShell (MonadShell (..))
import ShellRun.Data.Command (Command (..))
import ShellRun.Data.Legend (LegendErr (..), LegendMap)
import ShellRun.Logging (MonadLogger (..))
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
      MonadWriter [Text],
      MonadLogger
    )
    via MockShellBase

instance MonadShell NoLegendMockShell where
  -- Purposely giving a bad shell function here to prove that no legend skips
  -- this (otherwise would die here)
  legendPathToMap :: Text -> NoLegendMockShell (Either LegendErr LegendMap)
  legendPathToMap _ = pure $ Left $ EntryErr "Bad key"

  runCommands :: [Command] -> NoLegendMockShell ()
  runCommands = MTL.tell . fmap command

instance Show a => Show (NoLegendMockShell a) where
  show _ = "MkNoLegendMockShell"
