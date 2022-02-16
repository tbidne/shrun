{-# LANGUAGE RecordWildCards #-}

-- | Module that provides env types and requisite typeclasses, along with
-- parsing functionality.
--
-- @since 0.1.0.0
module ShellRun.Env
  ( -- * \"HasX\" style typeclasses
    HasCommands (..),
    HasCommandDisplay (..),
    HasCommandLogging (..),
    HasFileLogging (..),
    HasLegend (..),
    HasTimeout (..),

    -- * Types
    Env (..),
    CommandDisplay (..),
    CommandLogging (..),

    -- * Functions
    runParser,
    displayCommand,
  )
where

import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Control.Monad.STM qualified as STM
import Options.Applicative qualified as OApp
import ShellRun.Args (Args (..))
import ShellRun.Args qualified as Args
import ShellRun.Command (Command (..))
import ShellRun.Env.Types
  ( CommandDisplay (..),
    CommandLogging (..),
    Env (..),
    HasCommandDisplay (..),
    HasCommandLogging (..),
    HasCommands (..),
    HasFileLogging (..),
    HasLegend (..),
    HasTimeout (..),
  )
import ShellRun.Logging.Queue (LogTextQueue (..))
import ShellRun.Prelude

-- | Runs the parser.
--
-- @since 0.1.0.0
runParser :: IO Env
runParser = do
  args <- OApp.execParser Args.parserInfoArgs
  fl <- case aFileLogging args of
    Nothing -> pure Nothing
    Just fp -> do
      queue <- STM.atomically $ TBQueue.newTBQueue 1000
      pure $ Just (fp, MkLogTextQueue queue)
  pure $ toEnv fl args

toEnv :: Maybe (Tuple2 FilePath LogTextQueue) -> Args -> Env
toEnv fl MkArgs {..} =
  MkEnv
    aLegend
    aTimeout
    fl
    aCommandLogging
    aCommandDisplay
    aCommands

-- | Returns the key if one exists and we pass in 'ShowKey', otherwise
-- returns the command.
--
-- ==== __Examples__
-- >>> displayCommand ShowKey (MkCommand Nothing "cmd")
-- "cmd"
--
-- >>> displayCommand ShowCommand (MkCommand (Just "key") "cmd")
-- "cmd"
--
-- >>> displayCommand ShowKey (MkCommand (Just "key") "cmd")
-- "key"
--
-- @since 0.1.0.0
displayCommand :: CommandDisplay -> Command -> Text
displayCommand ShowKey (MkCommand (Just key) _) = key
displayCommand _ (MkCommand _ cmd) = cmd
