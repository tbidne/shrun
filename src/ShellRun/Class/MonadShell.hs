-- | Provides the `MonadShell` typeclass.
--
-- @since 0.1.0.0
module ShellRun.Class.MonadShell
  ( MonadShell (..),
    runShell,
  )
where

import ShellRun.Command (Command (..))
import ShellRun.Command qualified as Command
import ShellRun.Data.NonEmptySeq (NonEmptySeq)
import ShellRun.Env (HasCommands (..), HasLegend (..))
import ShellRun.Legend (LegendErr, LegendMap)
import ShellRun.Logging.Log (Log (..), LogLevel (..), LogMode (..))
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import ShellRun.Prelude

-- | The core typeclass for @shell-run@.
--
-- @since 0.1.0.0
type MonadShell :: (Type -> Type) -> Constraint
class Monad m => MonadShell m where
  -- | Given a filepath, attempts to read and parse the file into
  -- a `LegendMap`.
  --
  -- @since 0.1.0.0
  legendPathToMap :: FilePath -> m (Either LegendErr LegendMap)

  -- | Runs commands.
  --
  -- @since 0.1.0.0
  runCommands :: NonEmptySeq Command -> m ()

-- | `runShell` is the entry point for running shell commands, i.e.,
-- `MonadShell` instances.
--
-- @since 0.1.0.0
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
  NonEmptySeq Text ->
  m (Either LegendErr (NonEmptySeq Command))
maybePathToCommands Nothing cmds = pure $ Right $ fmap (MkCommand Nothing) cmds
maybePathToCommands (Just path) cmds = do
  lMap <- legendPathToMap path
  pure $ lMap >>= (`Command.translateCommands` cmds)

runCommandsOrLogErr ::
  (MonadShell m, RegionLogger m) =>
  Either LegendErr (NonEmptySeq Command) ->
  m ()
runCommandsOrLogErr (Right cmds) = runCommands cmds
runCommandsOrLogErr (Left err) = putLog $ MkLog errTxt Fatal Finish
  where
    errTxt = "Error parsing legend file: " <> showt err
