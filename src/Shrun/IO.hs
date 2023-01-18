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
import Data.Sequence ((<|))
import Data.Text qualified as T
import Data.Time.Relative (RelativeTime)
import Effects.MonadTime (withTiming)
import Effects.System.MonadProcess qualified as P
import Shrun.Configuration.Env.Types
  ( HasAnyError (getAnyError),
    HasCommands (..),
    HasLogging (..),
  )
import Shrun.Data.Command (Command (..))
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
  (MonadProcess m, MonadSTM m) =>
  Command ->
  m (ExitCode, Stdout, Stderr)
shExitCode (MkCommand _ cmd) = do
  (exitCode, stdout, stderr) <- P.readProcess process
  pure (exitCode, wrap MkStdout stdout, wrap MkStderr stderr)
  where
    process = P.shell (T.unpack cmd)
    wrap f = f . T.strip . decodeUtf8Lenient . BSL.toStrict

-- | Version of 'shExitCode' that returns 'Left' 'Stderr' if there is a failure,
-- 'Right' 'Stdout' otherwise.
--
-- @since 0.1
tryShExitCode ::
  (MonadProcess m, MonadSTM m) =>
  Command ->
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
  ( MonadProcess m,
    MonadSTM m
  ) =>
  Command ->
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
    HasLogging env (Region m),
    MonadHandleReader m,
    MonadIORef m,
    MonadMask m,
    MonadProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
    MonadTime m
  ) =>
  -- | Command to run.
  Command ->
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
                  U.whenJust mFileLogging (`logFile` log)
              )
              cmd

  withTiming (cmdFn command) >>= \case
    (rt, Nothing) -> do
      -- update completed commands
      completedCmds <- asks getCompletedCmds
      modifyTVarM' completedCmds (command <|)

      pure $ Right $ U.timeSpecToRelTime rt
    (rt, Just err) -> do
      -- update completed commands
      completedCmds <- asks getCompletedCmds
      modifyTVarM' completedCmds (command <|)

      -- update anyError
      anyError <- asks getAnyError
      writeTVarM anyError True

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
  ( MonadHandleReader m,
    MonadIORef m,
    MonadMask m,
    MonadProcess m,
    MonadSTM m
  ) =>
  -- | Function to apply to streamed logs.
  (Log -> m ()) ->
  -- | Command to run.
  Command ->
  -- | Error, if any. Note that this will be 'Just' iff the command exited
  -- with an error, even if the error message itself is blank.
  m (Maybe Stderr)
tryCommandStream logFn cmd@(MkCommand _ cmdTxt) = do
  let outSpec = P.createPipe
      errSpec = P.createPipe
      procConfig =
        P.setStderr outSpec $
          P.setStdout errSpec $
            P.shell (T.unpack cmdTxt)

  (exitCode, lastReadErr) <-
    P.withProcessWait procConfig $ \p -> streamOutput logFn cmd p

  pure $ case exitCode of
    ExitSuccess -> Nothing
    ExitFailure _ -> Just $ readHandleResultToStderr lastReadErr

streamOutput ::
  ( MonadCatch m,
    MonadHandleReader m,
    MonadIORef m,
    MonadSTM m
  ) =>
  -- | Function to apply to streamed logs.
  (Log -> m ()) ->
  -- | Command to run.
  Command ->
  -- | Process handle.
  Process () Handle Handle ->
  -- | Exit code along w/ any leftover data.
  m (ExitCode, ReadHandleResult)
streamOutput logFn cmd p = do
  -- lastReadRef stores the last message in case it is the final error
  -- message.
  lastReadErrRef <- newIORef Nothing
  exitCode <- U.untilJust $ do
    -- We need to read from both stdout and stderr -- regardless of if we
    -- created a single pipe in tryCommandStream -- or else we will miss
    -- messages
    outResult <- readHandle (P.getStdout p)
    errResult <- readHandle (P.getStderr p)
    writeLog logFn cmd lastReadErrRef outResult
    writeLog logFn cmd lastReadErrRef errResult

    P.getExitCode p

  -- try to get final data (stderr)
  lastReadErr <- readIORef lastReadErrRef
  remainingData <- readHandle (P.getStdout p)

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
  MonadIORef m =>
  (Log -> m ()) ->
  Command ->
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
