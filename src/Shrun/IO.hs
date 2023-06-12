-- | Provides the low-level `IO` functions for running shell commands.
module Shrun.IO
  ( -- * Stdout/stderr newtypes
    Stderr (..),

    -- * Running commands
    tryCommandLogging,
  )
where

import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
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
  ( CommandResult (..),
    ReadHandleResult (..),
    Stderr (..),
    readHandle,
    readHandleResultToStderr,
  )
import Shrun.Logging.Formatting (formatConsoleLog, formatFileLog)
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (..))
import Shrun.Logging.Types
  ( Log (..),
    LogLevel (..),
    LogMode (..),
    LogRegion (LogRegion),
  )
import Shrun.Prelude
import Shrun.Utils qualified as U
import System.Exit (ExitCode (..))

-- | Runs the command, returns ('ExitCode', 'Stderr')
shExitCode ::
  ( HasInit env,
    MonadProcess m,
    MonadReader env m,
    MonadSTM m
  ) =>
  CommandP1 ->
  m (ExitCode, Stderr)
shExitCode cmd = do
  process <- commandToProcess cmd <$> asks getInit
  (exitCode, _stdout, stderr) <- P.readProcess process
  pure (exitCode, wrap MkStderr stderr)
  where
    wrap f = f . T.strip . decodeUtf8Lenient . BSL.toStrict

-- | Version of 'shExitCode' that returns 'Left' 'Stderr' if there is a failure,
-- 'Right' 'Stdout' otherwise.
tryShExitCode ::
  ( HasInit env,
    MonadProcess m,
    MonadReader env m,
    MonadSTM m
  ) =>
  CommandP1 ->
  m (Maybe Stderr)
tryShExitCode cmd =
  shExitCode cmd <&> \case
    (ExitSuccess, _) -> Nothing
    (ExitFailure _, stderr) -> Just stderr

-- | Runs the command, returning the time elapsed along with a possible
-- error.
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
  -- | Result.
  m CommandResult
tryCommandLogging command = do
  -- NOTE: We do not want tryCommandLogging to throw sync exceptions, as that
  -- will take down the whole app. tryCommandStream and tryShExitCode should be
  -- total, but there are still a few functions here that can throw. To wit:
  --
  -- - atomically: Used in prependCompletedCommand, setAnyErrorTrue,
  --               writeTBQueueA.
  -- - getSystemTimeString: Used in formatFileLog.
  --
  -- We could catch these exceptions and simply print an error. However, both
  -- of these errors have nothing to do with the actual command that is being
  -- run and point to something wrong with shrun itself. Morever, "recovery"
  -- in these instances is unclear, as we are either dropping logs (how do we
  -- report these errors?) or failing to get the time (how should we log?).
  --
  -- Thus the most reasonable course of action is to let shrun die and print
  -- the actual error so it can be fixed.

  logging <- asks getLogging
  let keyHide = logging ^. #keyHide

  let cmdFn = case (logging ^. #cmdLog, logging ^. #fileLog) of
        -- 1. No CmdLogging and no FileLogging: No streaming at all.
        (Nothing, Nothing) -> tryShExitCode
        -- 2. No CmdLogging but FileLogging: Stream (to file) but no console
        --    region.
        (Nothing, Just fileLogging) -> \cmd -> do
          let logFn :: Log -> m ()
              logFn = logFile keyHide fileLogging

          logFn hello

          tryCommandStream logFn cmd
        -- 3. CmdLogging: Create region and stream. Also stream to file if
        --    requested.
        (Just _, mFileLogging) -> \cmd ->
          withRegion Linear $ \region -> do
            let logFn log = do
                  logConsole logging region log
                  for_ mFileLogging (\fl -> logFile keyHide fl log)

            logFn hello

            tryCommandStream logFn cmd

  withTiming (cmdFn command) >>= \case
    (rt, Nothing) -> do
      -- update completed commands
      prependCompletedCommand command

      pure $ CommandSuccess $ U.timeSpecToRelTime rt
    (rt, Just err) -> do
      -- update completed commands
      prependCompletedCommand command

      -- update anyError
      setAnyErrorTrue

      pure $ CommandFailure (U.timeSpecToRelTime rt) err
  where
    logConsole logging region log = do
      let consoleQueue = logging ^. #consoleLog
          formatted = formatConsoleLog logging log
      writeTBQueueA consoleQueue (LogRegion (log ^. #mode) region formatted)

    logFile keyHide fileLogging log = do
      formatted <- formatFileLog keyHide fileLogging log
      writeTBQueueA (fileLogging ^. #log % _2) formatted

    hello =
      MkLog
        { cmd = Just command,
          msg = "Starting...",
          lvl = LevelCommand,
          mode = LogModeSet
        }

-- | Similar to 'tryCommand' except we attempt to stream the commands' output
-- instead of the usual swallowing.
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
    P.withProcessWait procConfig (streamOutput logFn cmd)

  pure $ case exitCode of
    ExitSuccess -> Nothing
    ExitFailure _ -> Just $ readHandleResultToStderr lastReadErr

-- NOTE: This was an attempt to set the buffering so that we could use
-- hGetLine. Unfortunately that failed, see Note
-- [Blocking / Streaming output]. Leaving this here as documentation.
--
--  where
--    -- copy of P.createPipe except we set the buffering
--    createPipe' = P.mkPipeStreamSpec $ \_ h -> do
--      hSetBuffering h NoBuffering
--      pure (h, hClose h)

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
      sleepFn = when (pollInterval /= 0) (microsleep pollInterval)
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

  -- Try to get final data. The semigroup prioritizes errors and then the LHS
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
--
-- See Note [EOF / blocking error]
writeLog ::
  (MonadIORef m) =>
  (Log -> m ()) ->
  CommandP1 ->
  IORef (Maybe ReadHandleResult) ->
  ReadHandleResult ->
  m ()
writeLog _ _ _ (ReadErr _) = pure ()
writeLog _ _ _ ReadNoData = pure ()
writeLog logFn cmd lastReadRef (ReadSuccess messages) = do
  writeIORef lastReadRef (Just (ReadSuccess messages))
  for_ messages $ \msg ->
    logFn $
      MkLog
        { cmd = Just cmd,
          msg,
          lvl = LevelCommand,
          mode = LogModeSet
        }
