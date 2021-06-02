-- | Provides the `MonadShell` typeclass.
module ShellRun.Class.MonadShell
  ( MonadShell (..),
  )
where

import Data.Text (Text)
import ShellRun.Types.Command (Command (..))
import ShellRun.Types.Legend (LegendErr, LegendMap)

-- | The core typeclass for `ShellRun`.
class Monad m => MonadShell m where
  -- | Given a filepath, attempts to read and parse the file into
  -- a `LegendMap`.
  legendPathToMap :: Text -> m (Either LegendErr LegendMap)

  -- | Runs commands.
  runCommands :: [Command] -> m ()
