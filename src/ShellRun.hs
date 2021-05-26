{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun
  ( ShellT (..),
    runShell,
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Reader (MonadIO, MonadReader, MonadTrans, ReaderT)
import Control.Monad.Reader qualified as MTL
import Data.Text (Text)
import Data.Text qualified as T
import ShellRun.Async qualified as ShAsync
import ShellRun.Class.MonadLogger (MonadLogger)
import ShellRun.Class.MonadLogger qualified as ML
import ShellRun.Class.MonadShell (MonadShell (..))
import ShellRun.Parsing.Commands qualified as ParseCommands
import ShellRun.Parsing.Legend qualified as ParseLegend
import ShellRun.Types.Command (Command (..))
import ShellRun.Types.Env (Env (..))
import ShellRun.Types.Legend (LegendErr)

newtype ShellT e m a = MkShellT {runShellT :: ReaderT e m a}
  deriving (Functor, Applicative, Monad, MonadReader e, MonadIO, MonadLogger, MonadTrans, MonadUnliftIO)

instance (MonadIO m, MonadLogger m, MonadUnliftIO m) => MonadShell (ShellT Env m) where
  legendPathToMap = MTL.liftIO . ParseLegend.legendPathToMap
  runCommands = ShAsync.runCommands

runShell :: (MonadReader Env m, MonadLogger m, MonadShell m) => m ()
runShell = do
  legend <- MTL.asks legend
  commands <- MTL.asks commands
  parsedCommands <- maybePathToCommands legend commands
  runCommandsOrLogErr parsedCommands

maybePathToCommands :: MonadShell m => Maybe Text -> [Text] -> m (Either LegendErr [Command])
maybePathToCommands Nothing commands = pure $ Right $ fmap MkCommand commands
maybePathToCommands (Just path) commands = do
  lMap <- legendPathToMap path
  pure $ lMap >>= (`ParseCommands.translateCommands` commands)

runCommandsOrLogErr :: (MonadLogger m, MonadShell m) => Either LegendErr [Command] -> m ()
runCommandsOrLogErr (Right cmds) = runCommands cmds
runCommandsOrLogErr (Left err) = ML.logError errTxt
  where
    errTxt = "Error parsing legend file: " <> T.pack (show err)