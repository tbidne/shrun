{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides the `runCommands` function used for running a list
--   of commands asynchronously.
module ShellRun.Async
  ( runCommands,
  )
where

import Control.Concurrent qualified as Concurrent
import Control.Exception (IOException)
import Control.Exception qualified as Except
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Loops qualified as Loops
import Control.Monad.Reader (MonadIO, MonadReader)
import Control.Monad.Reader qualified as MTL
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Text qualified as T
import ShellRun.Class.Has (HasSubLogging (..), HasTimeout (..))
import ShellRun.Class.MonadLogger (LogLevel (..), LogMode (..), MonadLogger)
import ShellRun.Class.MonadLogger qualified as ML
import ShellRun.IO qualified as ShIO
import ShellRun.Math (NonNegative, RAdd (..))
import ShellRun.Math qualified as Math
import ShellRun.Types.Command (Command (..))
import ShellRun.Types.Env (SubLogging (..))
import ShellRun.Types.IO (Stderr (..))
import ShellRun.Utils qualified as U
import System.Clock (Clock (..))
import System.Clock qualified as C
import UnliftIO qualified
import UnliftIO.Async qualified as UAsync

-- | Runs all commands asynchronously while printing out a running counter.
-- When a command finishes/crashes, stdout is updated with the result. If
-- a haskell exception is encountered in @shell-run@ /itself/, this is
-- considered a fatal error and all threads are killed.
runCommands ::
  ( HasSubLogging env,
    HasTimeout env,
    MonadIO m,
    MonadLogger m,
    MonadUnliftIO m,
    MonadReader env m
  ) =>
  [Command] ->
  m ()
runCommands commands = do
  start <- MTL.liftIO $ C.getTime Monotonic
  let actions = UAsync.mapConcurrently_ runCommand commands
  let actionsWithTimer = UAsync.race_ actions counter

  result :: Either IOException () <- UnliftIO.withRunInIO $ \runner -> Except.try $ runner actionsWithTimer

  case result of
    Left ex ->
      ML.logFatal $
        T.pack $
          "Encountered an exception. This is likely not an error in any of the "
            <> "commands run but rather an error in ShellRun itself: "
            <> Except.displayException ex
    Right _ -> pure ()

  end <- MTL.liftIO $ C.getTime Monotonic
  let totalTime = U.diffTime start end
  ML.clearLine
  ML.logInfoBlue "Finished!"
  ML.logInfoBlue $ "Total time elapsed: " <> U.formatTime totalTime

runCommand ::
  ( HasSubLogging env,
    MonadReader env m,
    MonadIO m
  ) =>
  Command ->
  m ()
runCommand command@(MkCommand cmd) = do
  subLogging <- MTL.asks getSubLogging
  let shFn = case subLogging of
        None -> ShIO.tryTimeSh
        Combine -> ShIO.tryTimeShCombineStdout
        Native -> ShIO.tryTimeShNativeStdout

  MTL.liftIO $ do
    res <- shFn command Nothing
    (seconds, logFn, msg) <- case res of
      Left (t, MkStderr err) -> pure (t, ML.logError, err)
      Right t -> pure (t, ML.logInfoSuccess, "Successfully ran `" <> cmd <> "`")
    ML.clearLine
    logFn msg
    logFn $ "Time elapsed: " <> U.formatTime seconds <> "\n"

counter :: (HasSubLogging env, HasTimeout env, MonadLogger m, MonadReader env m, MonadIO m) => m ()
counter = do
  timeout <- MTL.asks getTimeout
  timer <- MTL.liftIO $ IORef.newIORef $ Math.unsafeNonNegative 0
  let inc = Math.unsafeNonNegative 1
  Loops.whileM_ (keepRunning timer timeout) $ do
    elapsed <- MTL.liftIO $ do
      Concurrent.threadDelay 1_000_000
      IORef.modifyIORef' timer (+:+ inc)
      IORef.readIORef timer
    logCounter elapsed

logCounter ::
  ( HasSubLogging env,
    MonadReader env m,
    MonadLogger m
  ) =>
  NonNegative ->
  m ()
logCounter elapsed = do
  subLogging <- MTL.asks getSubLogging
  case subLogging of
    Native -> do
      -- If we are logging subprocesses then everything gets a new line.
      -- We are not going to bother with trying to make the counter "update",
      -- i.e., overwrite via carriage returns.
      ML.clear
      ML.logLevelMode InfoCyan Line $ "Running time: " <> U.formatTime elapsed
    _ -> do
      ML.resetCR
      ML.logLevelMode InfoCyan NoLine $ "Running time: " <> U.formatTime elapsed

keepRunning :: (MonadIO m, MonadLogger m) => IORef NonNegative -> Maybe NonNegative -> m Bool
keepRunning timer to = do
  elapsed <- MTL.liftIO $ IORef.readIORef timer
  if timedOut elapsed to
    then do
      ML.clearLine
      ML.logWarn "Timed out, cancelling remaining tasks."
      pure False
    else pure True

timedOut :: NonNegative -> Maybe NonNegative -> Bool
timedOut timer =
  \case
    Nothing -> False
    Just t -> timer > t
