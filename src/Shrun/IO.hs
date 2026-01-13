{-# LANGUAGE ViewPatterns #-}

-- | Provides the low-level `IO` functions for running shell commands.
module Shrun.IO
  ( -- * Types
    CommandResult (..),
    Stderr (..),

    -- * Running commands
    tryCommandLogging,

    -- * Misc
    killChildPids,
    killPids,
  )
where

import Control.Monad (filterM)
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time.Relative (RelativeTime)
import Effects.FileSystem.HandleWriter qualified as HW
import Effects.System.Process (Pid, ProcessHandle)
import Effects.System.Process qualified as P
import Effects.Time (MonadTime (getMonotonicTime))
import Shrun.Command.Types
  ( CommandP1,
    CommandStatus (CommandFailure, CommandRunning, CommandSuccess),
    commandToProcess,
  )
import Shrun.Configuration.Data.CommandLogging
  ( BufferLength,
    BufferTimeout,
    ReportReadErrorsSwitch,
  )
import Shrun.Configuration.Data.CommandLogging.ReadStrategy
  ( ReadStrategy
      ( ReadBlock,
        ReadBlockLineBuffer
      ),
  )
import Shrun.Configuration.Env.Types
  ( HasAnyError,
    HasCommandLogging (getCommandLogging),
    HasCommands (getCleanup),
    HasCommonLogging (getCommonLogging),
    HasConsoleLogging (getConsoleLogging),
    HasFileLogging (getFileLogging),
    HasInit (getInit),
    HasLogging,
    setAnyErrorTrue,
    updateCommandStatus,
  )
import Shrun.Data.Text (UnlinedText)
import Shrun.Data.Text qualified as ShrunText
import Shrun.IO.Handle
  ( HandleResult,
    ReadHandleResult (ReadErr, ReadErrSuccess, ReadNoData, ReadSuccess),
  )
import Shrun.IO.Handle qualified as Handle
import Shrun.Logging qualified as Logging
import Shrun.Logging.Formatting (formatConsoleLog, formatFileLog)
import Shrun.Logging.MonadRegionLogger
  ( MonadRegionLogger
      ( Region,
        withRegion
      ),
    restoreTimerRegion,
  )
import Shrun.Logging.Types
  ( Log (MkLog, cmd, lvl, mode, msg),
    LogLevel (LevelCommand),
    LogMode (LogModeSet),
    LogRegion (LogRegion),
  )
import Shrun.Logging.Types qualified as Types
import Shrun.Prelude
import Shrun.Utils qualified as U
import Text.Read qualified as TR

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
  = CommandResultSuccess RelativeTime
  | CommandResultFailure RelativeTime Stderr
  deriving stock (Eq, Show)

-- | Runs the command, returning the time elapsed along with a possible
-- error.
tryCommandLogging ::
  forall m env.
  ( HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasInit env,
    HasLogging env m,
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
  -- - atomically: Used in updateCommandStatus, setAnyErrorTrue,
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
  (consoleLogging, consoleLogQueue, timerRegion) <- asks getConsoleLogging
  mFileLogging <- asks getFileLogging

  -- NOTE: [Restore Timer Region]
  --
  -- Generally, we want the timer region to be the last console region, but
  -- sequential commands can screw with that. That is, if a sequential command
  -- spawns --command-logs, those seem to be placed in a new region _after_
  -- the timer log. That is not what we want.
  --
  -- To handle this, we call 'restoreTimerRegion' immediately after creating
  -- the new region (two places below). This seems to work, though it feels
  -- a bit shaky. We _could_ move this logic to the timer itself, which would
  -- potentially restore it every second. That should be quite robust
  -- (assuming the restore logic does what we want), but it's possibly
  -- wasteful, since experience seems to show it's only these single command
  -- logs that screw it up.
  --
  -- Hence for now, let's just do it the one time after commands are created.
  -- If we have problems, consider moving it to the timer.

  let keyHide = commonLogging ^. #keyHide
      consoleLogSwitch = consoleLogging ^. #commandLogging % #unConsoleLogCmdSwitch
      -- In general, our loggers take an optional region (for debugging) and
      -- a log, and send it off to the console / file queues, depending on
      -- the queue. Debugging gets its own queue because we do not want it
      -- to be overridden by command logs.
      cmdFn = case (consoleLogSwitch, mFileLogging) of
        -- 1. No CommandLogging and no FileLogging: No logging at all.
        (False, Nothing) -> tryCommandStream (\_ _ -> pure ())
        -- 2. CommandLogging but no FileLogging. Stream.
        (True, Nothing) -> \cmd ->
          withRegion Linear $ \cmdRegion -> do
            restoreTimerRegion timerRegion
            let logFn mRegion log =
                  let region = fromMaybe cmdRegion mRegion
                   in logConsole keyHide consoleLogQueue region consoleLogging log

            logFn Nothing hello

            tryCommandStream logFn cmd
        -- 3. No CommandLogging but FileLogging: Stream (to file) but no console
        --    region.
        (False, Just fileLogging) -> \cmd -> do
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
        (True, Just fileLogging) -> \cmd ->
          withRegion Linear $ \cmdRegion -> do
            restoreTimerRegion timerRegion
            let logFn mRegion log = do
                  let region = fromMaybe cmdRegion mRegion
                  logConsole keyHide consoleLogQueue region consoleLogging log
                  logFile keyHide fileLogging log

            logFn Nothing hello

            tryCommandStream logFn cmd

  withTiming (cmdFn command) >>= \case
    (rt, Nothing) -> do
      -- update completed commands
      updateCommandStatus command CommandSuccess

      pure $ CommandResultSuccess $ U.timeSpecToRelTime rt
    (rt, Just err) -> do
      -- update completed commands
      updateCommandStatus command CommandFailure

      -- update anyError
      setAnyErrorTrue

      pure $ CommandResultFailure (U.timeSpecToRelTime rt) err
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
    HasCommands env,
    HasLogging env m,
    MonadHandleReader m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadMask m,
    MonadProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
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
    -- Store the process PID and potential child PIDs. This is potentially
    -- needed for later cleanup, as the /bin/sh command may start children
    -- whose parents are later reassigned to PID 1.
    --
    -- See NOTE: [Command cleanup]
    mPid <- P.getPid ph
    childPids <- getChildPids True mPid
    updateCommandStatus cmd (CommandRunning (mPid, childPids))
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
              (IORef HandleResult)
              (IORef (Maybe UnlinedText))
              (m HandleResult)
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
  lastReadOut <- readIORef' lastReadOutRef
  lastReadErr <- readIORef' lastReadErrRef

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
  -- return the last output to give a good error message. We have two
  -- possible reads here:
  --
  -- 1. The last read while the process was running (lastReadErr)
  -- 2. A final read after the process exited (remainingErr)
  --
  -- We return everything, as timing issues means it is not always reliable
  -- which handle has which data. We sort the output according to the time
  -- they were read.
  --
  -- NB. It is not necessarily true that Err has the actual error, or that
  -- timing is respected. For instance, the command "nix run .#format"
  -- may first print a message about a dirty tree to stderr, then print
  -- the actual lint errors to stdout. Hence we actually want to show stdout,
  -- and this should be displayed /after/ stderr.
  --
  -- We therefore sort according to the timestamps in which each message
  -- was read, and display everything.
  --
  -- Do note that the ordering is not necessarily perfect, as it is based
  -- on when we read the handles, /not/ necessarily when the underlying
  -- command output that message. For instance, if the underlying command
  -- prints to stderr then stdout very quickly, we may read them in the same
  -- loop, in which case stdout will be given the earlier timestamp, as we
  -- arbitrarily choose to read it first above.
  let finalData =
        foldMap snd
          . L.sortOn fst
          $ [ lastReadOut,
              lastReadErr,
              remainingOut,
              remainingErr
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
        (IORef HandleResult)
        (IORef (Maybe UnlinedText))
        (m HandleResult)
    )
mkHandleParams blockSize readStrategy bufLength bufTimeout handle = do
  lastReadRef <- newIORef' (0, ReadNoData)
  prevReadRef <- newIORef' Nothing

  currTime <- getMonotonicTime
  bufFlushTimeRef <- newIORef' currTime

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
    MonadIORef m,
    MonadTime m
  ) =>
  -- | Block size.
  Int ->
  -- | Handle from which to read.
  Handle ->
  -- | Previous partial read.
  IORef (Maybe UnlinedText) ->
  -- | Result.
  m HandleResult
readFinalWithPrev blockSize handle prevReadRef = do
  readTime <- getMonotonicTime
  fmap (readTime,) $ Handle.readHandleRaw blockSize handle >>= \case
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
  IORef HandleResult ->
  HandleResult ->
  m ()
writeLog = \cases
  -- 1. No data: Do nothing.
  _ _ _ _ (_, ReadNoData) -> pure ()
  -- 2. ReadErr but ReadErrors is off: Do nothing.
  _ (getReadErrors -> False) _ _ (_, ReadErr _) -> pure ()
  -- 3. ReadErr and ReadErrors is on: Log it.
  logFn (getReadErrors -> True) cmd lastReadRef r@(_, ReadErr messages) ->
    writeLogHelper logFn cmd lastReadRef r messages
  -- 4. Log success and potentially errors.
  logFn reportReadErrors cmd lastReadRef r@(_, ReadErrSuccess errs successes) -> do
    when (getReadErrors reportReadErrors) $ writeLogHelper logFn cmd lastReadRef r errs
    writeLogHelper logFn cmd lastReadRef r successes
  -- 5. Log successes.
  logFn _ cmd lastReadRef r@(_, ReadSuccess messages) ->
    writeLogHelper logFn cmd lastReadRef r messages
  where
    getReadErrors = view #unReportReadErrorsSwitch
{-# INLINEABLE writeLog #-}

writeLogHelper ::
  ( HasCallStack,
    MonadIORef m
  ) =>
  (Log -> m b) ->
  CommandP1 ->
  IORef HandleResult ->
  HandleResult ->
  NonEmpty UnlinedText ->
  m ()
writeLogHelper logFn cmd lastReadRef handleResult messages = do
  writeIORef' lastReadRef handleResult
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
  Logging.logDebug $ \lvl -> do
    let cs = show $ P.cmdspec procConfig
        lg =
          MkLog
            { cmd = Just cmd,
              msg =
                Types.fromUnlined
                  $ "Command: '"
                  <> ShrunText.fromTextReplace (pack cs)
                  <> "'",
              lvl,
              mode = Types.LogModeFinish
            }
    withRegion Linear $ \r -> logFn r lg

killChildPids ::
  forall env m.
  ( HasCallStack,
    HasCommands env,
    HasLogging env m,
    MonadCatch m,
    MonadHandleWriter m,
    MonadProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
    MonadTime m
  ) =>
  Maybe Pid ->
  m ()
killChildPids Nothing = Logging.putDebugLogDirect "killChildPids: No pid given"
killChildPids (Just pid) = do
  pidsStr <- getChildPids False (Just pid)
  pidsToKill <- filterM canKillPid pidsStr
  killPids pidsToKill

getChildPids ::
  ( HasCallStack,
    HasCommands env,
    HasLogging env m,
    MonadCatch m,
    MonadHandleWriter m,
    MonadProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
    MonadTime m
  ) =>
  -- | Is multithreaded. Used for logging.
  Bool ->
  Maybe Pid ->
  m (List Pid)
getChildPids _ Nothing = pure []
getChildPids multiThreads (Just pid) = do
  asks getCleanup >>= \case
    Nothing -> pure []
    Just cleanup -> do
      (ec, stdout, stderr) <-
        readProcessTotal
          (cleanup ^. #findPidsExe)
          args
          "getChildPids"

      let (result, msg) = case ec of
            ExitFailure _ ->
              let m =
                    fromString
                      $ mconcat
                        [ "Failed finding child pids of '",
                          show pid,
                          "': out: '",
                          stdout,
                          "', err: '",
                          stderr,
                          "'"
                        ]
               in ([], m)
            ExitSuccess ->
              let pidsTxt =
                    T.lines
                      . T.strip
                      . pack
                      $ stdout
                  m =
                    fromString
                      $ mconcat
                        [ "Child pids of '",
                          show pid,
                          "': ",
                          unpack $ T.intercalate "," pidsTxt
                        ]
               in case traverse (TR.readMaybe . unpack) pidsTxt of
                    Nothing -> ([], fromString $ "Failed reading pid strings: " <> show pidsTxt)
                    Just pids -> (pids, m)
      logFn msg
      pure result
  where
    args = ["-P", show pid]

    logFn =
      -- If multiThreads is active then this function is possibly called from
      -- multiple threads i.e. the logs should be sent to the queue, as usual.
      --
      -- OTOH, this must have been called during termination when the queues
      -- are already shutdown, hence we should log directly.
      if multiThreads
        then Logging.putDebugLog
        else Logging.putDebugLogDirect

killPids ::
  ( HasCallStack,
    HasCommands env,
    HasLogging env m,
    MonadCatch m,
    MonadHandleWriter m,
    MonadProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadTime m
  ) =>
  List Pid ->
  m ()
killPids [] = pure ()
killPids pids =
  void
    . runKill "-15"
    $ pids

canKillPid ::
  forall env m.
  ( HasCallStack,
    HasCommands env,
    HasLogging env m,
    MonadCatch m,
    MonadHandleWriter m,
    MonadProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadTime m
  ) =>
  Pid ->
  m Bool
canKillPid = runKill "-0" . (: [])

runKill ::
  forall env m.
  ( HasCallStack,
    HasCommands env,
    HasLogging env m,
    MonadCatch m,
    MonadHandleWriter m,
    MonadProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadTime m
  ) =>
  String ->
  List Pid ->
  m Bool
runKill signal pids = do
  asks getCleanup >>= \case
    Nothing -> pure False
    Just cleanup -> do
      (ec, stdout, stderr) <-
        readProcessTotal
          (cleanup ^. #killPidsExe)
          (signal : pidArgs)
          ("runKill " <> signal)

      let msg = case ec of
            ExitSuccess ->
              fromString
                $ mconcat
                  [ "Successfully ran kill ",
                    signal,
                    " with: ",
                    pidDispStr
                  ]
            ExitFailure _ ->
              fromString
                $ mconcat
                  [ "Kill ",
                    signal,
                    " with '",
                    pidDispStr,
                    "' failed: ",
                    "': out: '",
                    stdout,
                    "', err: '",
                    stderr,
                    "'"
                  ]
      Logging.putDebugLogDirect msg

      case ec of
        ExitSuccess -> pure True
        ExitFailure _ -> pure False
  where
    pidArgs = show <$> pids
    pidDispStr = unpack $ T.intercalate ", " (showt <$> pids)

readProcessTotal ::
  ( HasCallStack,
    MonadCatch m,
    MonadProcess m
  ) =>
  FilePath ->
  [String] ->
  String ->
  m (ExitCode, String, String)
readProcessTotal exe args str = do
  tryMySync (P.readProcessWithExitCode exe args str) >>= \case
    Left ex -> pure (ExitFailure 1, "", mkExeErr exe args $ displayException ex)
    Right r -> pure r

mkExeErr :: String -> [String] -> String -> String
mkExeErr exeStr args err =
  mconcat
    [ "Failed running command '",
      exeStr,
      "' with args '",
      unpack $ T.intercalate "," (pack <$> args),
      "': ",
      err
    ]
