-- | This module provides the `runCommands` function used for running a list
--   of commands asynchronously.
module ShellRun.Async
  ( runCommands,
  )
where

import Control.Concurrent qualified as Concurrent
import Control.Exception (IOException)
import Control.Exception qualified as Except
import Control.Monad qualified as M
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Loops qualified as Loops
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Text qualified as T
import ShellRun.Data.Command (Command (..))
import ShellRun.Data.Env
  ( CommandLogging (..),
    HasCommandDisplay (..),
    HasCommandLogging (..),
    HasLogQueue (..),
    HasTimeout (..),
  )
import ShellRun.Data.IO (Stderr (..))
import ShellRun.IO qualified as ShIO
import ShellRun.Logging (Log (..), LogLevel (..), LogMode (..), MonadLogger (..))
import ShellRun.Logging qualified as Logging
import ShellRun.Math (NonNegative, RAdd (..))
import ShellRun.Math qualified as Math
import ShellRun.Prelude
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
  ( HasCommandDisplay env,
    HasLogQueue env,
    HasCommandLogging env,
    HasTimeout env,
    MonadIO m,
    MonadLogger m,
    MonadUnliftIO m,
    MonadReader env m
  ) =>
  [Command] ->
  m ()
runCommands commands = UAsync.withAsync printLogQueue $ \printer -> do
  logQueue <- asks getLogQueue
  start <- liftIO $ C.getTime Monotonic
  let actions = UAsync.mapConcurrently_ runCommand commands
      actionsWithTimer = UAsync.race_ actions counter

  result :: Either IOException () <- UnliftIO.withRunInIO $ \runner -> Except.try $ runner actionsWithTimer

  UAsync.cancel printer

  case result of
    Left ex -> do
      let errMsg =
            T.pack $
              "Encountered an exception. This is likely not an error in any of the "
                <> "commands run but rather an error in ShellRun itself: "
                <> Except.displayException ex
      Logging.writeQueue logQueue $ Logging.logFatal errMsg
    Right _ -> pure ()

  end <- liftIO $ C.getTime Monotonic
  let totalTime = U.diffTime start end
      totalTimeTxt = "Finished! Total time elapsed: " <> U.formatTime totalTime

  Logging.writeQueue logQueue $ Logging.logInfoBlue totalTimeTxt

  remainingLogs <- Logging.flushQueue logQueue
  traverse_ Logging.putLog remainingLogs

runCommand ::
  ( HasCommandDisplay env,
    HasLogQueue env,
    HasCommandLogging env,
    MonadReader env m,
    MonadIO m
  ) =>
  Command ->
  m ()
runCommand cmd = do
  commandDisplay <- asks getCommandDisplay
  logQueue <- asks getLogQueue
  commandLogging <- asks getCommandLogging
  let shFn = case commandLogging of
        Disabled -> ShIO.tryTimeSh commandDisplay
        Enabled -> ShIO.tryTimeShCommandOutput logQueue commandDisplay

  liftIO $ do
    res <- shFn cmd Nothing
    let lg = case res of
          Left (t, MkStderr err) ->
            let logTxt =
                  err
                    <> ". Time elapsed: "
                    <> U.formatTime t
             in Logging.logError logTxt
          Right t ->
            let name = U.displayCommand commandDisplay cmd
                logTxt =
                  "Successfully ran `"
                    <> name
                    <> "`. Time elapsed: "
                    <> U.formatTime t
             in Logging.logInfoSuccess logTxt
    Logging.writeQueue logQueue lg

counter ::
  ( HasLogQueue env,
    HasTimeout env,
    MonadLogger m,
    MonadReader env m,
    MonadIO m
  ) =>
  m ()
counter = do
  timeout <- asks getTimeout
  timer <- liftIO $ IORef.newIORef $ Math.unsafeNonNegative 0
  let inc = Math.unsafeNonNegative 1
  Loops.whileM_ (keepRunning timer timeout) $ do
    elapsed <- liftIO $ do
      Concurrent.threadDelay 1_000_000
      IORef.modifyIORef' timer (+:+ inc)
      IORef.readIORef timer
    logCounter elapsed

logCounter ::
  ( HasLogQueue env,
    MonadIO m,
    MonadReader env m
  ) =>
  NonNegative ->
  m ()
logCounter elapsed = do
  logQueue <- asks getLogQueue
  let lg =
        MkLog
          { msg = "Running time: " <> U.formatTime elapsed,
            lvl = InfoCyan,
            mode = CarriageReturn
          }
  Logging.writeQueue logQueue lg

keepRunning ::
  ( HasLogQueue env,
    MonadIO m,
    MonadReader env m
  ) =>
  IORef NonNegative ->
  Maybe NonNegative ->
  m Bool
keepRunning timer to = do
  elapsed <- liftIO $ IORef.readIORef timer
  if timedOut elapsed to
    then do
      logQueue <- asks getLogQueue
      Logging.writeQueue logQueue $
        Logging.logWarn "Timed out, cancelling remaining tasks."
      pure False
    else pure True

timedOut :: NonNegative -> Maybe NonNegative -> Bool
timedOut timer =
  \case
    Nothing -> False
    Just t -> timer > t

printLogQueue ::
  ( HasLogQueue env,
    MonadLogger m,
    MonadReader env m,
    MonadIO m
  ) =>
  m ()
printLogQueue = do
  logQueue <- asks getLogQueue
  M.forever $ Logging.readQueue logQueue >>= traverse_ Logging.putLog
