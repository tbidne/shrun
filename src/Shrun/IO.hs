-- | Provides the low-level `IO` functions for running shell commands.
--
-- @since 0.1
module Shrun.IO
  ( -- * Stdout/stderr newtypes
    Stdout (..),
    Stderr (..),

    -- * Running commands
    tryCommandLogging,
  )
where

import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Time.Relative (RelativeTime)
import Effects.Concurrent.Thread (microsleep)
import Effects.System.Process qualified as P
import Effects.Time (withTiming)
import Shrun.Configuration.Env.Types
  ( HasAnyError,
    HasCommands (..),
    HasInit (..),
    HasLogging (..),
    prependCompletedCommand,
    setAnyErrorTrue,
  )
import Shrun.Data.Command (CommandP1, commandToProcess)
import Shrun.IO.Types
  ( ReadHandleResult (..),
    Stderr (..),
    Stdout (..),
    readHandle,
    readHandleResultToStderr,
  )
import Shrun.Logging
  ( Log (..),
    LogLevel (..),
    LogMode (..),
    LogRegion (LogRegion),
    MonadRegionLogger (..),
    formatConsoleLog,
    formatFileLog,
  )
import Shrun.Prelude
import Shrun.Utils qualified as U
import System.Exit (ExitCode (..))

-- | Runs the command, returns ('ExitCode', 'Stdout', 'Stderr')
--
-- @since 0.1
shExitCode ::
  ( HasInit env,
    MonadProcess m,
    MonadReader env m,
    MonadSTM m
  ) =>
  CommandP1 ->
  m (ExitCode, Stdout, Stderr)
shExitCode cmd = do
  process <- commandToProcess cmd <$> asks getInit
  (exitCode, stdout, stderr) <- P.readProcess process
  pure (exitCode, wrap MkStdout stdout, wrap MkStderr stderr)
  where
    wrap f = f . T.strip . decodeUtf8Lenient . BSL.toStrict

-- | Version of 'shExitCode' that returns 'Left' 'Stderr' if there is a failure,
-- 'Right' 'Stdout' otherwise.
--
-- @since 0.1
tryShExitCode ::
  ( HasInit env,
    MonadProcess m,
    MonadReader env m,
    MonadSTM m
  ) =>
  CommandP1 ->
  m (Either Stderr Stdout)
tryShExitCode cmd = do
  (code, stdout, stderr) <- shExitCode cmd
  pure $ case code of
    ExitSuccess -> Right stdout
    ExitFailure _ -> Left stderr

-- | Version of 'tryShExitCode' that updated the completed commands.
-- On success, stdout is not returned.
--
-- @since 0.1
tryCommand ::
  ( HasInit env,
    MonadProcess m,
    MonadReader env m,
    MonadSTM m
  ) =>
  CommandP1 ->
  m (Maybe Stderr)
tryCommand cmd = preview _Left <$> tryShExitCode cmd

-- | Runs the command, returning the time elapsed along with a possible
-- error.
--
-- @since 0.7
tryCommandLogging ::
  forall m env.
  ( HasAnyError env,
    HasCommands env,
    HasInit env,
    HasLogging env (Region m),
    MonadHandleReader m,
    MonadIORef m,
    MonadMask m,
    MonadProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
    MonadThread m,
    MonadTime m
  ) =>
  -- | Command to run.
  CommandP1 ->
  -- | @'Left' (timeElapsed, error)@ if the command fails.
  -- @'Right' timeElapsed@ otherwise.
  m (Either (RelativeTime, Stderr) RelativeTime)
tryCommandLogging command = do
  logging <- asks getLogging

  let cmdFn = case (logging ^. #cmdLogging, logging ^. #fileLogging) of
        -- 1. No CmdLogging and no FileLogging: No streaming at all.
        (Nothing, Nothing) -> tryCommand
        -- 2. No CmdLogging but FileLogging: Stream (to file) but no console
        --    region.
        (Nothing, Just fileLogging) -> tryCommandStream (logFile fileLogging)
        -- 3. CmdLogging: Create region and stream. Also stream to file if
        --    requested.
        (Just _, mFileLogging) -> \cmd ->
          withRegion Linear $ \region ->
            tryCommandStream
              ( \log -> do
                  logConsole logging region log
                  for_ mFileLogging (`logFile` log)
              )
              cmd

  withTiming (cmdFn command) >>= \case
    (rt, Nothing) -> do
      -- update completed commands
      prependCompletedCommand command

      pure $ Right $ U.timeSpecToRelTime rt
    (rt, Just err) -> do
      -- update completed commands
      prependCompletedCommand command

      -- update anyError
      setAnyErrorTrue

      pure $ Left (U.timeSpecToRelTime rt, err)
  where
    logConsole logging region log = do
      let consoleQueue = logging ^. #consoleLogging
          formatted = formatConsoleLog logging log
      writeTBQueueM consoleQueue (LogRegion (log ^. #mode) region formatted)

    logFile fileLogging log = do
      formatted <- formatFileLog fileLogging log
      writeTBQueueM (fileLogging ^. #log % _2) formatted

-- | Similar to 'tryCommand' except we attempt to stream the commands' output
-- instead of the usual swallowing.
--
-- @since 0.1
tryCommandStream ::
  ( HasInit env,
    HasLogging env (Region m),
    MonadHandleReader m,
    MonadIORef m,
    MonadMask m,
    MonadProcess m,
    MonadReader env m,
    MonadSTM m,
    MonadThread m
  ) =>
  -- | Function to apply to streamed logs.
  (Log -> m ()) ->
  -- | Command to run.
  CommandP1 ->
  -- | Error, if any. Note that this will be 'Just' iff the command exited
  -- with an error, even if the error message itself is blank.
  m (Maybe Stderr)
tryCommandStream logFn cmd = do
  let outSpec = P.createPipe
      errSpec = P.createPipe

  procConfig <-
    asks getInit
      <&> P.setStderr outSpec
        . P.setStdout errSpec
        . commandToProcess cmd

  (exitCode, lastReadErr) <-
    P.withProcessWait procConfig $ \p -> streamOutput logFn cmd p

  pure $ case exitCode of
    ExitSuccess -> Nothing
    ExitFailure _ -> Just $ readHandleResultToStderr lastReadErr

streamOutput ::
  forall m env.
  ( HasLogging env (Region m),
    MonadCatch m,
    MonadHandleReader m,
    MonadIORef m,
    MonadReader env m,
    MonadSTM m,
    MonadThread m
  ) =>
  -- | Function to apply to streamed logs.
  (Log -> m ()) ->
  -- | Command that was run.
  CommandP1 ->
  -- | Process handle.
  Process () Handle Handle ->
  -- | Exit code along w/ any leftover data.
  m (ExitCode, ReadHandleResult)
streamOutput logFn cmd p = do
  -- lastReadRef stores the last message in case it is the final error
  -- message.
  lastReadErrRef <- newIORef Nothing
  logging <- asks (getLogging @env @(Region m))
  let pollInterval = logging ^. (#pollInterval % #unPollInterval)
      sleepFn =
        if pollInterval == 0
          then pure ()
          else microsleep pollInterval
  exitCode <- U.untilJust $ do
    -- We need to read from both stdout and stderr -- regardless of if we
    -- created a single pipe in tryCommandStream -- or else we will miss
    -- messages
    outResult <- readHandle (P.getStdout p)
    errResult <- readHandle (P.getStderr p)
    writeLog logFn cmd lastReadErrRef outResult
    writeLog logFn cmd lastReadErrRef errResult

    -- NOTE: IF we do not have a sleep here then the CPU blows up. Adding
    -- a delay helps keep the CPU reasonable.
    sleepFn

    P.getExitCode p

  -- Try to get final data. The semigroup prioritize errors and then the LHS
  -- for equal data constructors.
  lastReadErr <- readIORef lastReadErrRef
  remainingData <-
    (<>)
      <$> readHandle (P.getStderr p)
      <*> readHandle (P.getStdout p)

  pure $ (exitCode,) $ case lastReadErr of
    Nothing -> remainingData
    Just r -> remainingData <> r

-- We occasionally get invalid reads here -- usually when the command
-- exits -- likely due to a race condition. It would be nice to
-- prevent these entirely, but for now ignore them, as it does not
-- appear that we ever lose important messages.
--
-- EDIT: Possibly fixed by switch to typed-process and
-- https://github.com/fpco/typed-process/issues/25?
writeLog ::
  (MonadIORef m) =>
  (Log -> m ()) ->
  CommandP1 ->
  IORef (Maybe ReadHandleResult) ->
  ReadHandleResult ->
  m ()
writeLog _ _ _ (ReadErr _) = pure ()
writeLog _ _ _ ReadNoData = pure ()
writeLog logFn cmd lastReadRef (ReadSuccess msg) = do
  writeIORef lastReadRef (Just (ReadSuccess msg))
  logFn $
    MkLog
      { cmd = Just cmd,
        msg,
        lvl = LevelSubCommand,
        mode = LogModeSet
      }
