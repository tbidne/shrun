module ShellRun.Class.MonadShell
  ( MonadShell (..),
  )
where

import Data.Text (Text)
import ShellRun.Types.Command (Command (..))
import ShellRun.Types.Legend (LegendErr, LegendMap)

class Monad m => MonadShell m where
  legendPathToMap :: Text -> m (Either LegendErr LegendMap)
  runCommands :: [Command] -> m ()