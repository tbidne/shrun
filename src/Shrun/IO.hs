-- | Provides the low-level `IO` functions for running shell commands.
module Shrun.IO
  ( -- * Types
    CommandResult (..),
    Stderr (..),

    -- * Running commands
    tryCommandLogging,
  )
where

import Data.Time.Relative (RelativeTime)
import Effects.FileSystem.HandleWriter qualified as HW
import Effects.System.Process (ProcessHandle)
import Effects.System.Process qualified as P
import Effects.Time (MonadTime (getMonotonicTime), withTiming)
import Shrun.Configuration.Data.CommandLogging
  ( BufferLength,
    BufferTimeout,
    ReportReadErrorsSwitch
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
import Shrun.Configuration.Data.CommonLogging.KeyHideSwitch
  ( KeyHideSwitch (KeyHideOff),
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
import Shrun.Logging.Types qualified as Types
import Shrun.Prelude
import Shrun.Utils qualified as U

-- | Newtype wrapper for stderr.
newtype Stderr = MkStderr {unStderr :: List UnlinedText}
  deriving stock (Eq, Show)

-- | Turns a 'ReadHandleResult' into a 'Stderr'.
readHandleResultToStderr :: ReadHandleResult -> Stderr
readHandleResultToStderr ReadNoData = MkStderr $ ShrunText.fromText "<No data>"
readHandleResultToStderr (ReadErr errs) = MkStderr (neToList errs)
readHandleResultToStderr (ReadSuccess errs) = MkStderr (neToList errs)
readHandleResultToStderr (ReadErrSuccess e1 e2) = MkStderr (neToList $ e1 <> e2)

-- | Result of running a command.
data CommandResult
  = CommandSuccess RelativeTime
  | CommandFailure RelativeTime Stderr
  deriving stock (Eq, Show)

-- | Runs the command, returns ('ExitCode', 'Stderr')
shExitCode ::
  ( HasCallStack,
    HasCommonLogging env,
    HasConsoleLogging env (Region m),
    HasInit env,
    MonadProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m
  ) =>
  CommandP1 ->
  m (ExitCode, Stderr)
shExitCode cmd = do
  process <- commandToProcess cmd <$> asks getInit

  logDebugCmd cmd process $ \region log -> do
    (consoleLogging, consoleLogQueue) <- asks getConsoleLogging
    let formatted = formatConsoleLog KeyHideOff consoleLogging log
    writeTBQueueA consoleLogQueue (LogRegion (log ^. #mode) region formatted)

  (exitCode, _stdout, stderr) <- P.readCreateProcessWithExitCode process ""
  pure (exitCode, wrap (MkStderr . ShrunText.fromText) stderr)
  where
    wrap f = f . pack
{-# INLINEABLE shExitCode #-}

-- | Version of 'shExitCode' that returns 'Left' 'Stderr' if there is a failure,
-- 'Right' 'Stdout' otherwise.
tryShExitCode ::
  ( HasCallStack,
    HasCommonLogging env,
    HasConsoleLogging env (Region m),
    HasInit env,
    MonadProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m
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
    MonadHandleWriter m,
    MonadIORef m,
    MonadProcess m,
    MonadMask m,
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

  commonLogging <- asks getCommonLogging
  (consoleLogging, consoleLogQueue) <- asks getConsoleLogging
  mFileLogging <- asks getFileLogging
  let keyHide = commonLogging ^. #keyHide
      -- In general, our loggers take an optional region (for debugging) and
      -- a log, and send it off to the console / file queues, depending on
      -- the queue. Debugging gets its own queue because we do not want it
      -- to be overridden by command logs.
      cmdFn = case (consoleLogging ^. #commandLogging, mFileLogging) of
        -- 1. No CommandLogging and no FileLogging: No streaming at all.
        (ConsoleLogCmdOff, Nothing) -> tryShExitCode
        -- 3. CommandLogging but no FileLogging. Stream.
        (ConsoleLogCmdOn, Nothing) -> \cmd ->
          withRegion Linear $ \cmdRegion -> do
            let logFn mRegion log =
                  let region = fromMaybe cmdRegion mRegion
                   in logConsole keyHide consoleLogQueue region consoleLogging log

            logFn Nothing hello

            tryCommandStream logFn cmd
        -- 3. No CommandLogging but FileLogging: Stream (to file) but no console
        --    region.
        (ConsoleLogCmdOff, Just fileLogging) -> \cmd -> do
          let logFn :: Maybe (Region m) -> Log -> m ()
              logFn mRegion log = do
                -- Even if cmdLogging is off, we still want to send debug
                -- logs, if enabled.
                for_ mRegion $ \region ->
                  logConsole keyHide consoleLogQueue region consoleLogging log

                logFile keyHide fileLogging log

          logFn Nothing hello

          tryCommandStream logFn cmd
        -- 4. CommandLogging and FileLogging: Stream (to both) and create console
        --    region.
        (ConsoleLogCmdOn, Just fileLogging) -> \cmd ->
          withRegion Linear $ \cmdRegion -> do
            let logFn mRegion log = do
                  let region = fromMaybe cmdRegion mRegion
                  logConsole keyHide consoleLogQueue region consoleLogging log
                  logFile keyHide fileLogging log

            logFn Nothing hello

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
    logConsole keyHide consoleQueue region consoleLogging log = do
      let formatted = formatConsoleLog keyHide consoleLogging log
      writeTBQueueA consoleQueue (LogRegion (log ^. #mode) region formatted)

    logFile keyHide fileLogging log = do
      formatted <- formatFileLog keyHide fileLogging log
      writeTBQueueA (fileLogging ^. #file % #queue) formatted

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
    HasCommonLogging env,
    MonadHandleReader m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadMask m,
    MonadProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadThread m,
    MonadTime m
  ) =>
  -- | Function to apply to streamed logs.
  (Maybe (Region m) -> Log -> m ()) ->
  -- | Command to run.
  CommandP1 ->
  -- | Error, if any. Note that this will be 'Just' iff the command exited
  -- with an error, even if the error message itself is blank.
  m (Maybe Stderr)
tryCommandStream logFn cmd = do
  (recvOutH, sendOutH) <- P.createPipe
  HW.hSetBuffering recvOutH HW.NoBuffering
  HW.hSetBuffering sendOutH HW.NoBuffering

  (recvErrH, sendErrH) <- P.createPipe
  HW.hSetBuffering recvErrH HW.NoBuffering
  HW.hSetBuffering sendErrH HW.NoBuffering

  -- NOTE: [process vs. typed-process]
  --
  -- We previously switched from process to typed-process. This came with
  -- some improvements (ByteString output rather than String, more robust
  -- wrt handle output). We now switch back to process. Why?
  --
  -- We encountered a bug where SIGINT only promptly killed shrun + subcommands
  -- when command logging was active. That is, consider the following steps:
  --
  -- 1. Run 'shrun "sleep 15"
  -- 3. Run 'ps aux | grep sleep' to get shrun's pid.
  -- 2. In a separate terminal, run 'kill -2 <shrun_pid>
  --
  -- We should see shrun exit immediately with a cancellation message,
  -- and a subsequent 'ps aux | grep sleep' should show no running processes.
  --
  -- Unfortunately, this only worked when command logging was active.
  -- In particular, the exception was blocked until the sleep subcommand
  -- finished (15 seconds), so nothing was actually cancelled. The reason
  -- has something to do with typed-process's readProcess not respecting
  -- async exceptions. There are a few suspicious bug reports:
  --
  -- - https://github.com/fpco/typed-process/issues/32
  -- - https://github.com/fpco/typed-process/issues/38
  -- - https://github.com/fpco/typed-process/issues/69
  --
  -- More generally, investigation revealed that typed-process uses
  -- unliftio's bracket and friends i.e uninterruptibleMask is involved.
  -- While I am unsure of the exact nature of the bug, I am not surprised
  -- async exceptions are going wrong in the presence of uninterruptibleMask.
  --
  -- Happily, process does _not_ appear to have this bug, and I believe
  -- unliftio's / safe-exception's choice of uninterruptibleMask is the
  -- wrong one regardless, thus I generally try to avoid them on principle.
  -- Hence this is an easy switch.
  --
  -- The switch from ByteString to String is sad, but perhaps this can be
  -- improved when process receives OsString support.
  let initToConfig :: Maybe Text -> CreateProcess
      initToConfig mInit =
        (commandToProcess cmd mInit)
          { P.std_out = P.UseHandle sendOutH,
            P.std_in = P.Inherit,
            P.std_err = P.UseHandle sendErrH,
            P.cwd = Nothing,
            -- We are possibly trying to read from these after the process
            -- closes (e.g. an error), so it is important they are not
            -- closed automatically!
            P.close_fds = False
          }

  procConfig <- initToConfig <$> asks getInit
  logDebugCmd cmd procConfig (logFn . Just)

  (exitCode, finalData) <- P.withCreateProcess procConfig $ \_ _ _ ph -> do
    streamOutput (logFn Nothing) cmd (recvOutH, recvErrH, ph)

  pure $ case exitCode of
    ExitSuccess -> Nothing
    ExitFailure _ -> Just $ readHandleResultToStderr finalData
{-# INLINEABLE tryCommandStream #-}

type ProcessParams = Tuple3 Handle Handle ProcessHandle

streamOutput ::
  forall m env.
  ( HasCallStack,
    HasCommandLogging env,
    MonadCatch m,
    MonadHandleReader m,
    MonadIORef m,
    MonadProcess m,
    MonadReader env m,
    MonadThread m,
    MonadTime m
  ) =>
  -- | Function to apply to streamed logs.
  (Log -> m ()) ->
  -- | Command that was run.
  CommandP1 ->
  -- | Running process params.
  ProcessParams ->
  -- | Exit code along w/ any leftover data.
  m (ExitCode, ReadHandleResult)
streamOutput logFn cmd processParams = do
  -- NOTE: [Saving final error message]
  --
  -- We want to save the final error message if it exists, so that we can
  -- report it to the user. Programs can be inconsistent where they report
  -- errors, so we read both stdout and stderr, prioritizing the latter when
  -- both exist.
  commandLogging <- asks getCommandLogging

  let bufferLength = commandLogging ^. #bufferLength
      bufferTimeout = commandLogging ^. #bufferTimeout
      reportReadErrors = commandLogging ^. #reportReadErrors

      pollInterval :: Natural
      pollInterval = commandLogging ^. (#pollInterval % #unPollInterval)

      sleepFn :: m ()
      sleepFn = when (pollInterval /= 0) (microsleep pollInterval)

      blockSize :: Int
      blockSize = commandLogging ^. (#readSize % #unReadSize % _MkBytes)

      readStrategy = commandLogging ^. #readStrategy

      handleToParams ::
        Handle ->
        m
          ( Tuple3
              (IORef ReadHandleResult)
              (IORef (Maybe UnlinedText))
              (m ReadHandleResult)
          )
      handleToParams =
        mkHandleParams
          blockSize
          readStrategy
          bufferLength
          bufferTimeout

  -- - lastReadXRef: The result of the last read for handle X.
  --
  -- - prevReadXRef: Whatever was leftover from the last read for handle X.
  --   This is part of the "read block line buffer" strategy i.e. only
  --   contains "partial data" from the previous read when it exists
  --   and we are using that strategy. Compare to lastReadXRef, which
  --   __always__ contains the results for the last read, for the purposes
  --   of error reporting.
  --
  -- - readBlockX: Function for reading from handle X.

  (lastReadOutRef, prevReadOutRef, readBlockOut) <- handleToParams outHandle
  (lastReadErrRef, prevReadErrRef, readBlockErr) <- handleToParams errHandle

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

    P.getProcessExitCode processHandle

  -- These are the final reads while the process was running.
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
        (,)
          <$> readFinalWithPrev blockSize outHandle prevReadOutRef
          <*> readFinalWithPrev blockSize errHandle prevReadErrRef

  writeLog logFn reportReadErrors cmd lastReadOutRef remainingOut
  writeLog logFn reportReadErrors cmd lastReadErrRef remainingErr

  -- NOTE: [Stderr reporting]
  --
  -- In the event of a process failure (exitCode == ExitFailure), we want to
  -- return the last (successful) read, as it is the most likely to have
  -- relevant information. We have two possible reads here:
  --
  -- 1. The last read while the process was running (lastReadErr)
  -- 2. A final read after the process exited (remainingErr)
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
  where
    (outHandle, errHandle, processHandle) = processParams
{-# INLINEABLE streamOutput #-}

-- | Create params for reading from the handle.
mkHandleParams ::
  ( HasCallStack,
    MonadCatch m,
    MonadHandleReader m,
    MonadIORef m,
    MonadTime m
  ) =>
  -- | Read block size.
  Int ->
  -- | Read strategy.
  ReadStrategy ->
  -- | Max buffer length, for read-block-line-buffer strategy.
  BufferLength ->
  -- | Max buffer time, for read-block-line-buffer strategy.
  BufferTimeout ->
  -- | Handle from which to read.
  Handle ->
  -- | Returns:
  --
  --  1. Ref for the last read (always active).
  --  2. Ref for previous partial read (only for read-block-line-buffer
  --     strategy).
  --  3. Read function.
  m
    ( Tuple3
        (IORef ReadHandleResult)
        (IORef (Maybe UnlinedText))
        (m ReadHandleResult)
    )
mkHandleParams blockSize readStrategy bufLength bufTimeout handle = do
  lastReadRef <- newIORef ReadNoData
  prevReadRef <- newIORef Nothing

  currTime <- getMonotonicTime
  bufFlushTimeRef <- newIORef currTime

  let readFn = case readStrategy of
        ReadBlock -> Handle.readHandle Nothing blockSize handle
        ReadBlockLineBuffer ->
          let outBufferParams = (prevReadRef, bufLength, bufTimeout, bufFlushTimeRef)
           in Handle.readHandle (Just outBufferParams) blockSize handle

  pure (lastReadRef, prevReadRef, readFn)
{-# INLINEABLE mkHandleParams #-}

-- | Final read after the process has exited, to retrieve leftover data.
-- Only used with the read-block-line-buffer strategy.
readFinalWithPrev ::
  ( HasCallStack,
    MonadCatch m,
    MonadHandleReader m,
    MonadIORef m
  ) =>
  -- | Block size.
  Int ->
  -- | Handle from which to read.
  Handle ->
  -- | Previous partial read.
  IORef (Maybe UnlinedText) ->
  -- | Result.
  m ReadHandleResult
readFinalWithPrev blockSize handle prevReadRef = do
  Handle.readHandleRaw blockSize handle >>= \case
    -- Do not care about errors here, since we may still have leftover
    -- data that we need to get. If we cared, we could log the errors
    -- here, but it seems minor.
    Left _ -> Handle.readAndUpdateRefFinal prevReadRef ""
    Right bs -> Handle.readAndUpdateRefFinal prevReadRef bs
{-# INLINEABLE readFinalWithPrev #-}

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
  NonEmpty UnlinedText ->
  m ()
writeLogHelper logFn cmd lastReadRef handleResult messages = do
  writeIORef lastReadRef handleResult
  for_ messages $ \msg ->
    logFn
      $ MkLog
        { cmd = Just cmd,
          msg = Types.fromUnlined msg,
          lvl = LevelCommand,
          mode = LogModeSet
        }
{-# INLINEABLE writeLogHelper #-}

logDebugCmd ::
  ( HasCommonLogging r,
    MonadReader r m,
    MonadRegionLogger m
  ) =>
  CommandP1 ->
  CreateProcess ->
  (Region m -> Log -> m ()) ->
  m ()
logDebugCmd cmd procConfig logFn = do
  commonLogging <- asks getCommonLogging
  when (commonLogging ^. #debug % #unDebug) $ do
    let cs = show $ P.cmdspec procConfig
        lg =
          MkLog
            { cmd = Just cmd,
              msg =
                Types.fromUnlined
                  $ "Command: '"
                  <> ShrunText.fromTextReplace (pack cs)
                  <> "'",
              lvl = Types.LevelDebug,
              mode = Types.LogModeFinish
            }
    withRegion Linear $ \r -> logFn r lg
