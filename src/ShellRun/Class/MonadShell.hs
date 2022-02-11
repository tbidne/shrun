-- | Provides the `MonadShell` typeclass.
module ShellRun.Class.MonadShell
  ( MonadShell (..),
  )
where

import ShellRun.Data.Command (Command (..))
import ShellRun.Data.Legend (LegendErr, LegendMap)
import ShellRun.Prelude

-- | The core typeclass for @shell-run@.
type MonadShell :: (Type -> Type) -> Constraint
class Monad m => MonadShell m where
  -- | Given a filepath, attempts to read and parse the file into
  -- a `LegendMap`.
  legendPathToMap :: Text -> m (Either LegendErr LegendMap)

  -- | Runs commands.
  runCommands :: List Command -> m ()
