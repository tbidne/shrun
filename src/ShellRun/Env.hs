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
    HasCommandTruncation (..),
    HasFileLogging (..),
    HasLegend (..),
    HasTimeout (..),

    -- * Types
    Env (..),
    CommandDisplay (..),
    CommandLogging (..),
    CommandTruncation (..),

    -- * Functions
    runParser,
    displayCommand,
    displayCommandTruncation,
  )
where

import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Control.Monad.STM qualified as STM
import Data.Text qualified as T
import Options.Applicative qualified as OApp
import ShellRun.Args (Args (..))
import ShellRun.Args qualified as Args
import ShellRun.Command (Command (..))
import ShellRun.Data.InfNum (PosInfNum (..))
import ShellRun.Env.Types
  ( CommandDisplay (..),
    CommandLogging (..),
    CommandTruncation (..),
    Env (..),
    HasCommandDisplay (..),
    HasCommandLogging (..),
    HasCommandTruncation (..),
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
    { legend = aLegend,
      timeout = aTimeout,
      fileLogging = fl,
      commandLogging = aCommandLogging,
      commandDisplay = aCommandDisplay,
      commandTruncation = aCommandTruncation,
      commands = aCommands
    }

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

-- | Displays a command, taking truncation into account.
--
-- ==== __Examples__
-- >>> displayCommandTruncation (MkCommandTruncation 10) ShowCommand (MkCommand Nothing "this is a long command")
--
-- @since 0.1.0.0
displayCommandTruncation :: CommandTruncation -> CommandDisplay -> Command -> Text
displayCommandTruncation (MkCommandTruncation PPosInf) disp cmd = displayCommand disp cmd
displayCommandTruncation (MkCommandTruncation (PFin n)) disp (MkCommand mkey cmd) =
  displayCommand disp (MkCommand key' cmd')
  where
    truncate = truncateText (fromIntegral n)
    cmd' = truncate cmd
    key' = fmap truncate mkey

truncateText :: Int -> Text -> Text
truncateText n txt
  | T.length txt <= n = txt
  | otherwise = txt'
  where
    txt' = T.take (n - 3) txt <> "..."
