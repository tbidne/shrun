module MockShell.BadLegendMockShell (BadLegendMockShell (..)) where

import Data.Text (Text)
import MockShell.MockShellBase (MockShellBase (..))
import ShellRun.Class.MonadLogger (MonadLogger (..))
import ShellRun.Class.MonadShell (MonadShell (..))
import ShellRun.Types.Args (Args (..))
import ShellRun.Types.Command (Command (..))
import ShellRun.Types.Legend (LegendErr (..), LegendMap)
import ShellRun.Types.NonNegative (NonNegative)

newtype BadLegendMockShell a = MkBadLegendMockShell (MockShellBase a)
  deriving (Show, Semigroup, Monoid) via MockShellBase a
  deriving (Functor, Applicative, Monad) via MockShellBase

instance MonadShell BadLegendMockShell where
  parseArgs :: BadLegendMockShell Args
  parseArgs = pure $ MkArgs (Just "path") Nothing []

  legendPathToMap :: Text -> BadLegendMockShell (Either LegendErr LegendMap)
  legendPathToMap _ = pure $ Left $ FileErr "File not found"

  runCommands :: [Command] -> Maybe NonNegative -> BadLegendMockShell ()
  runCommands _ _ = pure ()

instance MonadLogger BadLegendMockShell where
  logNoLine :: Text -> BadLegendMockShell ()
  logNoLine t = MkBadLegendMockShell $ MkMockShellBase () [t]

  logLine :: Text -> BadLegendMockShell ()
  logLine t = MkBadLegendMockShell $ MkMockShellBase () [t <> "\n"]
