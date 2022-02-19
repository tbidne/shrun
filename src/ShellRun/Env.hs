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
    HasCmdTruncation (..),
    HasFileLogging (..),
    HasLegend (..),
    HasTimeout (..),

    -- * Types
    Env (..),
    CommandDisplay (..),
    CommandLogging (..),
    Truncation (..),
    TruncationArea (..),

    -- * Functions
    runParser,
  )
where

import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Control.Monad.STM qualified as STM
import Options.Applicative qualified as OApp
import ShellRun.Args (ALineTruncation (..), Args (..))
import ShellRun.Args qualified as Args
import ShellRun.Data.InfNum (PosInfNum (..))
import ShellRun.Env.Types
  ( CommandDisplay (..),
    CommandLogging (..),
    Env (..),
    HasCmdTruncation (..),
    HasCommandDisplay (..),
    HasCommandLogging (..),
    HasCommands (..),
    HasFileLogging (..),
    HasLegend (..),
    HasTimeout (..),
    Truncation (..),
    TruncationArea (..),
  )
import ShellRun.Logging.Queue (LogTextQueue (..))
import ShellRun.Prelude
import System.Console.Terminal.Size (Window (..))
import System.Console.Terminal.Size qualified as TSize
import System.Exit qualified as SysExit

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
  toEnv fl args

toEnv :: Maybe (Tuple2 FilePath LogTextQueue) -> Args -> IO Env
toEnv fl MkArgs {..} = do
  lineTruncation' <- case aLineTruncation of
    Undetected x -> pure x
    Detected ->
      (width <<$>> TSize.size) >>= \case
        Just h -> pure $ MkTruncation (PFin h)
        Nothing -> SysExit.die "Failed trying to detect terminal size"
  pure $
    MkEnv
      { legend = aLegend,
        timeout = aTimeout,
        fileLogging = fl,
        commandLogging = aCommandLogging,
        commandDisplay = aCommandDisplay,
        cmdTruncation = aCmdTruncation,
        lineTruncation = lineTruncation',
        commands = aCommands
      }
