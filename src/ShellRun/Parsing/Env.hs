{-# LANGUAGE RecordWildCards #-}

-- | Parses command line  into the core 'Env' type used by the main
-- application.
module ShellRun.Parsing.Env
  ( runParser,
  )
where

import Options.Applicative qualified as OptApp
import ShellRun.Data.Env (Env (..))
import ShellRun.Parsing.Args (Args (..))
import ShellRun.Parsing.Args qualified as Args
import ShellRun.Prelude

-- | Runs the parser.
runParser :: IO Env
runParser = do
  args <- OptApp.execParser Args.parserInfoArgs
  pure $ toEnv args

toEnv :: Args -> Env
toEnv MkArgs {..} =
  MkEnv
    aLegend
    aTimeout
    aCommandLogging
    aCommandDisplay
    aCommands
