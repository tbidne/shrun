{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

-- | Parses command line  into the core 'Env' type used by the main
-- application.
module ShellRun.Parsing.Env
  ( runParser,
  )
where

import Control.Concurrent.STM.TBQueue (TBQueue)
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Control.Monad.STM qualified as STM
import Options.Applicative qualified as OptApp
import ShellRun.Data.Env (Env (..))
import ShellRun.Logging (Log, LogQueue (..))
import ShellRun.Parsing.Args (Args (..))
import ShellRun.Parsing.Args qualified as Args

-- | Runs the parser.
runParser :: IO Env
runParser = do
  queue <- STM.atomically $ TBQueue.newTBQueue 1000
  args <- OptApp.execParser Args.parserInfoArgs
  pure $ toEnv queue args

toEnv :: TBQueue Log -> Args -> Env
toEnv queue MkArgs {..} =
  MkEnv
    aLegend
    aTimeout
    aCommandLogging
    aCommandDisplay
    (MkLogQueue queue)
    aCommands