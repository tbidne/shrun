-- | Provides the low-level `IO` functions for running shell commands.
module Shrun.IO
  ( -- * Types
    CommandResult (..),
    Stderr (..),

    -- * Running commands
    tryCommandLogging,
  )
where

import Data.ByteString.Lazy qualified as BSL
import Data.Time.Relative (RelativeTime)
import Effects.Concurrent.Thread (microsleep)
import Effects.Process.Typed qualified as P
import Effects.Time (MonadTime (getMonotonicTime), withTiming)
import Shrun.Configuration.Data.CommandLogging
  ( ReportReadErrorsSwitch
      ( ReportReadErrorsOff,
        ReportReadErrorsOn
      ),
  )
import Shrun.Configuration.Data.CommandLogging.ReadStrategy
  ( ReadStrategy
      ( ReadBlock,
        ReadBlockLineBuffer
      ),
  )
import Shrun.Configuration.Data.ConsoleLogging
  ( ConsoleLogCmdSwitch
      ( ConsoleLogCmdOff,
        ConsoleLogCmdOn
      ),
  )
import Shrun.Configuration.Env.Types
  ( HasAnyError,
    HasCommandLogging (getCommandLogging),
    HasCommands,
    HasCommonLogging (getCommonLogging),
    HasConsoleLogging (getConsoleLogging),
    HasFileLogging (getFileLogging),
    HasInit (getInit),
    prependCompletedCommand,
    setAnyErrorTrue,
  )
import Shrun.Data.Command (CommandP1, commandToProcess)
import Shrun.Data.Text (UnlinedText)
import Shrun.Data.Text qualified as ShrunText
import Shrun.IO.Handle
  ( ReadHandleResult (ReadErr, ReadErrSuccess, ReadNoData, ReadSuccess),
  )
import Shrun.IO.Handle qualified as Handle
import Shrun.Logging.Formatting (formatConsoleLog, formatFileLog)
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (Region, withRegion))
import Shrun.Logging.Types
  ( Log (MkLog, cmd, lvl, mode, msg),
    LogLevel (LevelCommand),
    LogMode (LogModeSet),
    LogRegion (LogRegion),
  )
import Shrun.Prelude
import Shrun.Utils qualified as U

-- | Newtype wrapper for stderr.
newtype Stderr = MkStderr {unStderr :: List UnlinedText}
  deriving stock (Eq, Show)

-- | Turns a 'ReadHandleResult' into a 'Stderr'.
readHandleResultToStderr :: ReadHandleResult -> Stderr
readHandleResultToStderr ReadNoData = MkStderr $ ShrunText.fromText "<No data>"
readHandleResultToStderr (ReadErr errs) = MkStderr errs
readHandleResultToStderr (ReadSuccess errs) = MkStderr errs
readHandleResultToStderr (ReadErrSuccess e1 e2) = MkStderr (e1 <> e2)

-- | Result of running a command.
data CommandResult
  = CommandSuccess RelativeTime
  | CommandFailure RelativeTime Stderr
  deriving stock (Eq, Show)

-- | Runs the command, returns ('ExitCode', 'Stderr')
shExitCode ::
  ( HasCallStack,
    HasInit env,
    MonadReader env m,
    MonadTypedProcess m
  ) =>
  CommandP1 ->
  m (ExitCode, Stderr)
shExitCode cmd = do
  process <- commandToProcess cmd <$> asks getInit
  (exitCode, _stdout, stderr) <- P.readProcess process
  pure (exitCode, wrap (MkStderr . ShrunText.fromText) stderr)
  where
    wrap f = f . decodeUtf8Lenient . BSL.toStrict
{-# INLINEABLE shExitCode #-}

-- | Version of 'shExitCode' that returns 'Left' 'Stderr' if there is a failure,
-- 'Right' 'Stdout' otherwise.
tryShExitCode ::
  ( HasCallStack,
    HasInit env,
    MonadReader env m,
    MonadTypedProcess m
  ) =>
  CommandP1 ->
  m (Maybe Stderr)
tryShExitCode cmd =
  shExitCode cmd <&> \case
    (ExitSuccess, _) -> Nothing
    (ExitFailure _, stderr) -> Just stderr
{-# INLINEABLE tryShExitCode #-}

-- | Runs the command, returning the time elapsed along with a possible
-- error.
tryCommandLogging ::
  forall m env.
  ( HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasInit env,
    HasCommandLogging env,
    HasCommonLogging env,
    HasConsoleLogging env (Region m),
    HasFileLogging env,
    MonadHandleReader m,
    MonadIORef m,
    MonadMask m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
    MonadThread m,
    MonadTime m,
    MonadTypedProcess m
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

  commonLogging <- asks getCommonLogging
  (consoleLogging, consoleLogQueue) <- asks getConsoleLogging
  mFileLogging <- asks getFileLogging
  let keyHide = commonLogging ^. #keyHide

  let cmdFn = case (consoleLogging ^. #commandLogging, mFileLogging) of
        -- 1. No CommandLogging and no FileLogging: No streaming at all.
        (ConsoleLogCmdOff, Nothing) -> tryShExitCode
        -- 3. CommandLogging but no FileLogging. Stream.
        (ConsoleLogCmdOn, Nothing) -> \cmd ->
          withRegion Linear $ \region -> do
            let logFn = logConsole keyHide consoleLogQueue region consoleLogging
                {-# INLINEABLE logFn #-}

            logFn hello

            tryCommandStream logFn cmd
        -- 3. No CommandLogging but FileLogging: Stream (to file) but no console
        --    region.
        (ConsoleLogCmdOff, Just fileLogging) -> \cmd -> do
          let logFn :: Log -> m ()
              logFn = logFile keyHide fileLogging
              {-# INLINEABLE logFn #-}

          logFn hello

          tryCommandStream logFn cmd
        -- 4. CommandLogging and FileLogging: Stream (to both) and create console
        --    region.
        (ConsoleLogCmdOn, Just fileLogging) -> \cmd ->
          withRegion Linear $ \region -> do
            let logFn log = do
                  logConsole keyHide consoleLogQueue region consoleLogging log
                  logFile keyHide fileLogging log
                {-# INLINEABLE logFn #-}

            logFn hello

            tryCommandStream logFn cmd
      {-# INLINEABLE cmdFn #-}

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
    logConsole keyHide consoleQueue region consoleLogging log = do
      let formatted = formatConsoleLog keyHide consoleLogging log
      writeTBQueueA consoleQueue (LogRegion (log ^. #mode) region formatted)
    {-# INLINEABLE logConsole #-}

    logFile keyHide fileLogging log = do
      formatted <- formatFileLog keyHide fileLogging log
      writeTBQueueA (fileLogging ^. #file % #queue) formatted
    {-# INLINEABLE logFile #-}

    hello =
      MkLog
        { cmd = Just command,
          msg = "Starting...",
          lvl = LevelCommand,
          mode = LogModeSet
        }
{-# INLINEABLE tryCommandLogging #-}

-- | Similar to 'tryCommand' except we attempt to stream the commands' output
-- instead of the usual swallowing.
tryCommandStream ::
  ( HasInit env,
    HasCallStack,
    HasCommandLogging env,
    MonadHandleReader m,
    MonadIORef m,
    MonadMask m,
    MonadReader env m,
    MonadThread m,
    MonadTime m,
    MonadTypedProcess m
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

  (exitCode, finalData) <-
    P.withProcessWait procConfig (streamOutput logFn cmd)

  pure $ case exitCode of
    ExitSuccess -> Nothing
    ExitFailure _ -> Just $ readHandleResultToStderr finalData
{-# INLINEABLE tryCommandStream #-}

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
  ( HasCallStack,
    HasCommandLogging env,
    MonadCatch m,
    MonadHandleReader m,
    MonadIORef m,
    MonadReader env m,
    MonadThread m,
    MonadTime m,
    MonadTypedProcess m
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
  -- NOTE: [Saving final error message]
  --
  -- We want to save the final error message if it exists, so that we can
  -- report it to the user. Programs can be inconsistent where they report
  -- errors, so we read both stdout and stderr, prioritizing the latter when
  -- both exist.
  lastReadOutRef <- newIORef ReadNoData
  lastReadErrRef <- newIORef ReadNoData
  commandLogging <- asks getCommandLogging

  prevReadOutRef <- newIORef Nothing
  prevReadErrRef <- newIORef Nothing

  currTime <- getMonotonicTime

  bufferFlushOutTimeRef <- newIORef currTime
  bufferFlushErrTimeRef <- newIORef currTime

  let bufferLength = commandLogging ^. #bufferLength
      bufferTimeout = commandLogging ^. #bufferTimeout
      reportReadErrors = commandLogging ^. #reportReadErrors

      pollInterval :: Natural
      pollInterval = commandLogging ^. (#pollInterval % #unPollInterval)

      sleepFn :: m ()
      sleepFn = when (pollInterval /= 0) (microsleep pollInterval)
      {-# INLINEABLE sleepFn #-}

      blockSize :: Int
      blockSize =
        unsafeConvertIntegral
          $ commandLogging
          ^. (#readSize % #unReadSize % _MkBytes)

      readBlockOut :: m ReadHandleResult
      readBlockErr :: m ReadHandleResult
      (readBlockOut, readBlockErr) = case commandLogging ^. #readStrategy of
        ReadBlock ->
          ( Handle.readHandle Nothing blockSize (P.getStdout p),
            Handle.readHandle Nothing blockSize (P.getStderr p)
          )
        ReadBlockLineBuffer ->
          let outBufferParams = (prevReadOutRef, bufferLength, bufferTimeout, bufferFlushOutTimeRef)
              errBufferParams = (prevReadErrRef, bufferLength, bufferTimeout, bufferFlushErrTimeRef)
           in ( Handle.readHandle (Just outBufferParams) blockSize (P.getStdout p),
                Handle.readHandle (Just errBufferParams) blockSize (P.getStderr p)
              )
      {-# INLINEABLE readBlockOut #-}
      {-# INLINEABLE readBlockErr #-}

  exitCode <- U.untilJust $ do
    -- We need to read from both stdout and stderr -- regardless of if we
    -- created a single pipe in tryCommandStream -- or else we will miss
    -- messages
    outResult <- readBlockOut
    errResult <- readBlockErr

    writeLog logFn reportReadErrors cmd lastReadOutRef outResult
    writeLog logFn reportReadErrors cmd lastReadErrRef errResult

    -- NOTE: IF we do not have a sleep here then the CPU blows up. Adding
    -- a delay helps keep the CPU reasonable.
    sleepFn

    P.getExitCode p

  -- Try to get final data.
  lastReadOut <- readIORef lastReadOutRef
  lastReadErr <- readIORef lastReadErrRef

  -- Leftover data. We need this as the process can exit before everything
  -- is read.
  (remainingOut, remainingErr) <- do
    -- This branch is really a paranoid "ensure we didn't change anything" if
    -- using the ReadBlock strategy. It is possible ReadBlockLineBuffer behaves
    -- the same most of the time; indeed, all of the tests pass with the
    -- normal ReadBlock strategy above even if we use ReadBlockLineBuffer
    -- below.
    case commandLogging ^. #readStrategy of
      ReadBlock -> (,) <$> readBlockOut <*> readBlockErr
      ReadBlockLineBuffer -> do
        let readRemaining toHandle ref =
              Handle.readHandleRaw blockSize (toHandle p) >>= \case
                -- Do not care about errors here, since we may still have leftover
                -- data that we need to get. If we cared, we could log the errors
                -- here, but it seems minor.
                Left _ -> Handle.readAndUpdateRefFinal ref ""
                Right bs -> Handle.readAndUpdateRefFinal ref bs
            {-# INLINEABLE readRemaining #-}

        (,)
          <$> readRemaining P.getStdout prevReadOutRef
          <*> readRemaining P.getStderr prevReadErrRef

  writeLog logFn reportReadErrors cmd lastReadOutRef remainingOut
  writeLog logFn reportReadErrors cmd lastReadErrRef remainingErr

  -- NOTE: [Stderr reporting]
  --
  -- In the event of a process failure (exitCode == ExitFailure), we want to
  -- return the last (successful) read, as it is the most likely to have
  -- relevant information. We have two possible reads here:
  --
  -- 1. The last read while the process was running (lastReadErr)
  -- 2. A final read after the process exited (remainingData)
  --
  -- We prioritize (Semigroup), in order:
  --
  -- 1. remainingErr
  -- 2. lastReadErr
  -- 3. remainingOut
  -- 4. lastReadOut
  --
  -- We make the assumption that the most recent Stderr is the most likely to
  -- have the relevant error message, though we fall back to stdout as this
  -- is not always true.
  let finalData =
        mconcat
          [ remainingErr,
            lastReadErr,
            remainingOut,
            lastReadOut
          ]

  pure (exitCode, finalData)
{-# INLINEABLE streamOutput #-}

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
  ( HasCallStack,
    MonadIORef m
  ) =>
  (Log -> m ()) ->
  ReportReadErrorsSwitch ->
  CommandP1 ->
  IORef ReadHandleResult ->
  ReadHandleResult ->
  m ()
writeLog _ _ _ _ ReadNoData = pure ()
writeLog _ ReportReadErrorsOff _ _ (ReadErr _) = pure ()
writeLog logFn ReportReadErrorsOn cmd lastReadRef r@(ReadErr messages) =
  writeLogHelper logFn cmd lastReadRef r messages
writeLog logFn reportReadErrors cmd lastReadRef r@(ReadErrSuccess errs successes) = do
  when (reportReadErrors == ReportReadErrorsOn) $ writeLogHelper logFn cmd lastReadRef r errs
  writeLogHelper logFn cmd lastReadRef r successes
writeLog logFn _ cmd lastReadRef r@(ReadSuccess messages) =
  writeLogHelper logFn cmd lastReadRef r messages
{-# INLINEABLE writeLog #-}

writeLogHelper ::
  ( HasCallStack,
    MonadIORef m
  ) =>
  (Log -> m b) ->
  CommandP1 ->
  IORef ReadHandleResult ->
  ReadHandleResult ->
  [UnlinedText] ->
  m ()
writeLogHelper logFn cmd lastReadRef handleResult messages = do
  writeIORef lastReadRef handleResult
  for_ messages $ \msg ->
    logFn
      $ MkLog
        { cmd = Just cmd,
          msg,
          lvl = LevelCommand,
          mode = LogModeSet
        }
{-# INLINEABLE writeLogHelper #-}
