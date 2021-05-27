module MockShell.BadLegendMockShell (BadLegendMockShell (..)) where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Writer (MonadWriter)
import Data.Text (Text)
import MockEnv (MockEnv)
import MockShell.MockShellBase (MockShellBase (..))
import ShellRun.Class.MonadLogger (MonadLogger (..))
import ShellRun.Class.MonadShell (MonadShell (..))
import ShellRun.Types.Command (Command (..))
import ShellRun.Types.Legend (LegendErr (..), LegendMap)

newtype BadLegendMockShell a = MkBadLegendMockShell {runBadLegendMockShell :: MockShellBase a}
  deriving (Functor, Applicative, Monad, MonadReader MockEnv, MonadWriter [Text], MonadLogger)

instance MonadShell BadLegendMockShell where
  legendPathToMap :: Text -> BadLegendMockShell (Either LegendErr LegendMap)
  legendPathToMap _ = pure $ Left $ FileErr "File not found"

  runCommands :: [Command] -> BadLegendMockShell ()
  runCommands _ = pure ()

instance Show a => Show (BadLegendMockShell a) where
  show _ = "MkBadLegendMockShell"
