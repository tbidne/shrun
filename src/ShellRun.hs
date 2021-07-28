-- | This module is the entry point to the @ShellRun@ library used by
-- the @ShellRun@ executable.
module ShellRun
  ( ShellT (..),
    runShell,
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Reader qualified as MTL
import ShellRun.Async qualified as ShAsync
import ShellRun.Class.MonadShell (MonadShell (..))
import ShellRun.Data.Command (Command (..))
import ShellRun.Data.Env (Env (..))
import ShellRun.Data.Legend (LegendErr, LegendMap)
import ShellRun.Env (HasCommands (..), HasLegend (..))
import ShellRun.Logging (MonadLogger)
import ShellRun.Logging qualified as Logging
import ShellRun.Parsing.Commands qualified as ParseCommands
import ShellRun.Parsing.Legend qualified as ParseLegend
import ShellRun.Prelude

-- | `ShellT` is the main application type that runs shell commands.
type ShellT :: Type -> (Type -> Type) -> Type -> Type
newtype ShellT e m a = MkShellT {runShellT :: ReaderT e m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader e,
      MonadIO,
      MonadLogger,
      MonadUnliftIO
    )
    via (ReaderT e m)
  deriving (MonadTrans) via (ReaderT e)

instance
  (MonadIO m, MonadLogger m, MonadUnliftIO m) =>
  MonadShell (ShellT Env m)
  where
  legendPathToMap :: Text -> ShellT Env m (Either LegendErr LegendMap)
  legendPathToMap = MTL.liftIO . ParseLegend.legendPathToMap

  runCommands :: [Command] -> ShellT Env m ()
  runCommands = ShAsync.runCommands

-- | `runShell` is the entry point for running shell commands, i.e.,
-- `MonadShell` instances.
runShell ::
  ( HasCommands env,
    HasLegend env,
    MonadReader env m,
    MonadLogger m,
    MonadShell m
  ) =>
  m ()
runShell = do
  legendMap <- MTL.asks getLegend
  cmds <- MTL.asks getCommands
  parsedCommands <- maybePathToCommands legendMap cmds
  runCommandsOrLogErr parsedCommands

maybePathToCommands ::
  MonadShell m =>
  Maybe Text ->
  [Text] ->
  m (Either LegendErr [Command])
maybePathToCommands Nothing cmds = pure $ Right $ fmap (MkCommand Nothing) cmds
maybePathToCommands (Just path) cmds = do
  lMap <- legendPathToMap path
  pure $ lMap >>= (`ParseCommands.translateCommands` cmds)

runCommandsOrLogErr ::
  (MonadLogger m, MonadShell m) =>
  Either LegendErr [Command] ->
  m ()
runCommandsOrLogErr (Right cmds) = runCommands cmds
runCommandsOrLogErr (Left err) = Logging.putLogFatal errTxt
  where
    errTxt = "Error parsing legend file: " <> showt err
