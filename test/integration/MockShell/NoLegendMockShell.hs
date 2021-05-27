{-# LANGUAGE ImportQualifiedPost #-}

module MockShell.NoLegendMockShell (NoLegendMockShell (..)) where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.Writer qualified as MTL
import Data.Text (Text)
import MockEnv (MockEnv)
import MockShell.MockShellBase (MockShellBase (..))
import ShellRun.Class.MonadLogger (MonadLogger (..))
import ShellRun.Class.MonadShell (MonadShell (..))
import ShellRun.Types.Command (Command (..))
import ShellRun.Types.Legend (LegendErr (..), LegendMap)

newtype NoLegendMockShell a = MkNoLegendMockShell {runNoLegendMockShell :: MockShellBase a}
  deriving (Functor, Applicative, Monad, MonadReader MockEnv, MonadWriter [Text], MonadLogger)

instance MonadShell NoLegendMockShell where
  -- Purposely giving a bad shell function here to prove that no legend skips
  -- this (otherwise would die here)
  legendPathToMap :: Text -> NoLegendMockShell (Either LegendErr LegendMap)
  legendPathToMap _ = pure $ Left $ EntryErr "Bad key"

  runCommands :: [Command] -> NoLegendMockShell ()
  runCommands = MTL.tell . fmap getCommand

instance Show a => Show (NoLegendMockShell a) where
  show _ = "MkNoLegendMockShell"
