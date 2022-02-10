{-# LANGUAGE UndecidableInstances #-}

-- | This module is the entry point to the @ShellRun@ library used by
-- the @ShellRun@ executable.
module ShellRun
  ( ShellT (..),
    runShell,
  )
where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import ShellRun.Async qualified as ShAsync
import ShellRun.Class.MonadShell (MonadShell (..))
import ShellRun.Data.Command (Command (..))
import ShellRun.Data.Env (Env (..))
import ShellRun.Data.Legend (LegendErr, LegendMap)
import ShellRun.Env (HasCommands (..), HasLegend (..))
import ShellRun.Logging.Log (Log (..), LogLevel (..), LogMode (..))
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import ShellRun.Parsing.Commands qualified as ParseCommands
import ShellRun.Parsing.Legend qualified as ParseLegend
import ShellRun.Prelude
import System.Console.Regions (ConsoleRegion)

-- | `ShellT` is the main application type that runs shell commands.
type ShellT :: Type -> (Type -> Type) -> Type -> Type
newtype ShellT e m a = MkShellT {runShellT :: ReaderT e m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader e,
      MonadCatch,
      MonadIO,
      MonadMask,
      MonadThrow,
      MonadUnliftIO,
      RegionLogger
    )
    via (ReaderT e m)
  deriving (MonadTrans) via (ReaderT e)

instance
  ( MonadIO m,
    MonadMask m,
    MonadUnliftIO m,
    RegionLogger m,
    Region m ~ ConsoleRegion
  ) =>
  MonadShell (ShellT Env m)
  where
  legendPathToMap :: Text -> ShellT Env m (Either LegendErr LegendMap)
  legendPathToMap = liftIO . ParseLegend.legendPathToMap

  runCommands :: [Command] -> ShellT Env m ()
  runCommands = ShAsync.runCommands

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
  Maybe Text ->
  [Text] ->
  m (Either LegendErr [Command])
maybePathToCommands Nothing cmds = pure $ Right $ fmap (MkCommand Nothing) cmds
maybePathToCommands (Just path) cmds = do
  lMap <- legendPathToMap path
  pure $ lMap >>= (`ParseCommands.translateCommands` cmds)

runCommandsOrLogErr ::
  (MonadShell m, RegionLogger m) =>
  Either LegendErr [Command] ->
  m ()
runCommandsOrLogErr (Right cmds) = runCommands cmds
runCommandsOrLogErr (Left err) = putLog $ MkLog errTxt Fatal Finish
  where
    errTxt = "Error parsing legend file: " <> showt err
