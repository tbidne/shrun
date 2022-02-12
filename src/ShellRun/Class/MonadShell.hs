-- | Provides the `MonadShell` typeclass.
module ShellRun.Class.MonadShell
  ( MonadShell (..),
    runShell,
  )
where

import ShellRun.Data.Command (Command (..))
import ShellRun.Data.Legend (LegendErr, LegendMap)
import ShellRun.Env (HasCommands (..), HasLegend (..))
import ShellRun.Logging.Log (Log (..), LogLevel (..), LogMode (..))
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import ShellRun.Parsing.Commands qualified as ParseCommands
import ShellRun.Prelude

-- | The core typeclass for @shell-run@.
type MonadShell :: (Type -> Type) -> Constraint
class Monad m => MonadShell m where
  -- | Given a filepath, attempts to read and parse the file into
  -- a `LegendMap`.
  legendPathToMap :: FilePath -> m (Either LegendErr LegendMap)

  -- | Runs commands.
  runCommands :: List Command -> m ()

-- | `runShell` is the entry point for running shell commands, i.e.,
-- `MonadShell` instances.
runShell ::
  ( HasCommands env,
    HasLegend env,
    MonadReader env m,
    MonadShell m,
    RegionLogger m
  ) =>
  m ()
runShell = do
  legendMap <- asks getLegend
  cmds <- asks getCommands
  parsedCommands <- maybePathToCommands legendMap cmds
  runCommandsOrLogErr parsedCommands

maybePathToCommands ::
  MonadShell m =>
  Maybe FilePath ->
  List Text ->
  m (Either LegendErr (List Command))
maybePathToCommands Nothing cmds = pure $ Right $ fmap (MkCommand Nothing) cmds
maybePathToCommands (Just path) cmds = do
  lMap <- legendPathToMap path
  pure $ lMap >>= (`ParseCommands.translateCommands` cmds)

runCommandsOrLogErr ::
  (MonadShell m, RegionLogger m) =>
  Either LegendErr (List Command) ->
  m ()
runCommandsOrLogErr (Right cmds) = runCommands cmds
runCommandsOrLogErr (Left err) = putLog $ MkLog errTxt Fatal Finish
  where
    errTxt = "Error parsing legend file: " <> showt err
