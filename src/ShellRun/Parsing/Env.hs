{-# LANGUAGE RecordWildCards #-}

-- | Parses command line  into the core 'Env' type used by the main
-- application.
module ShellRun.Parsing.Env
  ( runParser,
  )
where

import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Control.Monad.STM qualified as STM
import Options.Applicative qualified as OptApp
import ShellRun.Data.Env (Env (..))
import ShellRun.Logging.Queue (LogTextQueue (..))
import ShellRun.Parsing.Args (Args (..))
import ShellRun.Parsing.Args qualified as Args
import ShellRun.Prelude

-- | Runs the parser.
runParser :: IO Env
runParser = do
  args <- OptApp.execParser Args.parserInfoArgs
  fl <- case aFileLogging args of
    Nothing -> pure Nothing
    Just fp -> do
      queue <- STM.atomically $ TBQueue.newTBQueue 1000
      pure $ Just (fp, MkLogTextQueue queue)
  pure $ toEnv fl args

toEnv :: Maybe (FilePath, LogTextQueue) -> Args -> Env
toEnv fl MkArgs {..} =
  MkEnv
    aLegend
    aTimeout
    fl
    aCommandLogging
    aCommandDisplay
    aCommands
