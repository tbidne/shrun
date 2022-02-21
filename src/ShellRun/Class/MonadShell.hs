-- | Provides the `MonadShell` typeclass.
--
-- @since 0.1.0.0
module ShellRun.Class.MonadShell
  ( MonadShell (..),
    runShell,
  )
where

import ShellRun.Command (Command (..))
import System.FilePath ((</>))
import ShellRun.Data.FilePathDefault (FilePathDefault (..))
import ShellRun.Command qualified as Command
import ShellRun.Data.NonEmptySeq (NonEmptySeq)
import ShellRun.Env (HasCommands (..), HasLegend (..))
import ShellRun.Legend (LegendErr (..), LegendMap)
import ShellRun.Logging.Log (Log (..), LogDest (..), LogLevel (..), LogMode (..))
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import ShellRun.Prelude

-- | The core typeclass for @shell-run@.
--
-- @since 0.1.0.0
type MonadShell :: (Type -> Type) -> Constraint
class Monad m => MonadShell m where
  -- | Returns the default directory e.g. Xdg config dir.
  --
  -- @since 0.1.0.0
  getDefaultDir :: m FilePath

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
  (MonadShell m, RegionLogger m) =>
  FilePathDefault ->
  NonEmptySeq Text ->
  m (Either LegendErr (NonEmptySeq Command))
maybePathToCommands FPNone cmds = pure $ Right $ fmap (MkCommand Nothing) cmds
maybePathToCommands FPDefault cmds = do
  -- Search for the default legend
  defPath <- (</> "legend.txt") <$> getDefaultDir
  eMap <- legendPathToMap defPath
  case eMap of
    Left le -> case le of
      -- Because we search for the default legend file by default, we do not
      -- want to error if we do not find it (user may not have created it).
      -- Other errors are reported.
      FileErr _ -> do
        putLog $ MkLog
          { cmd = Nothing,
            msg = "No legend file found at: " <> showt defPath,
            lvl = Warn,
            mode = Append,
            dest = LogBoth
          }
        pure $ Right $ fmap (MkCommand Nothing) cmds
      other -> pure $ Left other
    Right lMap -> pure $ Command.translateCommands lMap cmds
maybePathToCommands (FPManual path) cmds = do
  -- Manually specified a legend file, all errors are fair game.
  lMap <- legendPathToMap path
  pure $ lMap >>= (`Command.translateCommands` cmds)

runCommandsOrLogErr ::
  (MonadShell m, RegionLogger m) =>
  Either LegendErr (NonEmptySeq Command) ->
  m ()
runCommandsOrLogErr (Right cmds) = runCommands cmds
runCommandsOrLogErr (Left err) = putLog log
  where
    errTxt = "Error parsing legend file: " <> showt err
    log =
      MkLog
        { cmd = Nothing,
          msg = errTxt,
          lvl = Fatal,
          mode = Append,
          dest = LogBoth
        }
