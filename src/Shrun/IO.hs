{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

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

import Data.ByteString qualified as BS
import Data.Sequence ((<|))
import Data.Text qualified as T
import Data.Time.Relative (RelativeTime)
import Effects.MonadFs (decodeUtf8Lenient)
import Effects.MonadTime (MonadTime (..), withTiming)
import GHC.IO.Handle (BufferMode (..))
import GHC.IO.Handle qualified as Handle
import Shrun.Configuration.Env.Types (HasCommands (..), HasLogging (..))
import Shrun.Data.Command (Command (..))
import Shrun.Data.Supremum (Supremum (..))
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
import System.Posix.IO.ByteString qualified as PBS
import System.Posix.Terminal qualified as PTerm
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..))
import System.Process qualified as P

-- | Newtype wrapper for stdout.
--
-- @since 0.1
newtype Stdout = MkStdout
  { -- | @since 0.1
    getStdout :: Text
  }

-- | @since 0.7
makeFieldLabelsNoPrefix ''Stdout

-- | Newtype wrapper for stderr.
--
-- @since 0.1
newtype Stderr = MkStderr
  { -- | @since 0.1
    getStderr :: Text
  }

-- | @since 0.7
makeFieldLabelsNoPrefix ''Stderr

makeStdErr :: Text -> Stderr
makeStdErr err = MkStderr $ "Error: '" <> T.strip err
{-# INLINE makeStdErr #-}

-- | Result from reading a handle. The ordering is based on:
--
-- @
-- 'ReadErr' _ < 'ReadNoData' < 'ReadSuccess'
-- @
--
-- The 'Semigroup' instance is based on this ordering, taking the greatest
-- element. For identical constructors, the left argument is taken.
--
-- @since 0.1
data ReadHandleResult
  = -- | Error encountered while trying to read a handle.
    --
    -- @since 0.1
    ReadErr Text
  | -- | Successfully read data from the handle.
    --
    -- @since 0.1
    ReadSuccess Text
  | -- | Successfully read no data from the handle.
    --
    -- @since 0.1
    ReadNoData
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.1
      Semigroup,
      -- | @since 0.1
      Monoid
    )
    via Supremum ReadHandleResult

-- | @since 0.1
instance Bounded ReadHandleResult where
  minBound = ReadErr ""
  {-# INLINEABLE minBound #-}
  maxBound = ReadSuccess ""
  {-# INLINEABLE maxBound #-}

-- | @since 0.1
instance Ord ReadHandleResult where
  compare x y | x == y = EQ
  compare (ReadSuccess _) _ = GT
  compare _ (ReadSuccess _) = LT
  compare ReadNoData _ = GT
  compare _ ReadNoData = LT
  compare (ReadErr _) _ = GT
  {-# INLINEABLE compare #-}

-- | Turns a 'ReadHandleResult' into a 'Stderr'.
--
-- @since 0.1
readHandleResultToStderr :: ReadHandleResult -> Stderr
readHandleResultToStderr ReadNoData = MkStderr "<No data>"
readHandleResultToStderr (ReadErr err) = MkStderr err
readHandleResultToStderr (ReadSuccess err) = MkStderr err
{-# INLINEABLE readHandleResultToStderr #-}

-- | Attempts to read from the handle.
--
-- @since 0.1
readHandle :: (MonadIO m) => Handle -> m ReadHandleResult
readHandle handle = do
  let displayEx :: Show a => Text -> a -> Text
      displayEx prefix =
        view #getStderr
          . makeStdErr
          . (<>) prefix
          . showt
      readEx = displayEx "Handle exception: "

  (isClosed, canRead) <-
    liftIO $
      (,)
        <$> Handle.hIsClosed handle
        <*> Handle.hIsReadable handle
  if
      | isClosed ->
          pure $ ReadErr $ displayEx @String "Handle closed" ""
      | not canRead ->
          pure $ ReadErr $ displayEx @String "Cannot read from handle" ""
      | otherwise -> do
          output :: Either SomeException ByteString <-
            liftIO $ tryAny $ BS.hGetNonBlocking handle blockSize
          let outDecoded = fmap decodeUtf8Lenient output
          case outDecoded of
            Left ex -> pure $ ReadErr $ readEx ex
            Right "" -> pure ReadNoData
            Right o -> pure $ ReadSuccess o
{-# INLINEABLE readHandle #-}

blockSize :: Int
blockSize = 1024
{-# INLINEABLE blockSize #-}

-- | Runs the command, returns ('ExitCode', 'Stdout', 'Stderr')
--
-- @since 0.1
shExitCode :: Command -> IO (ExitCode, Stdout, Stderr)
shExitCode (MkCommand _ cmd) = do
  (exitCode, stdout, stderr) <- P.readCreateProcessWithExitCode process ""
  pure (exitCode, wrap MkStdout stdout, wrap MkStderr stderr)
  where
    process = P.shell (T.unpack cmd)
    wrap f = f . T.strip . T.pack

-- | Version of 'shExitCode' that returns 'Left' 'Stderr' if there is a failure,
-- 'Right' 'Stdout' otherwise.
--
-- @since 0.1
tryShExitCode :: Command -> IO (Either Stderr Stdout)
tryShExitCode cmd = do
  (code, stdout, MkStderr err) <- shExitCode cmd
  pure $ case code of
    ExitSuccess -> Right stdout
    ExitFailure _ -> Left $ makeStdErr err

-- | Version of 'tryShExitCode' that updated the completed commands.
-- On success, stdout is not returned.
--
-- @since 0.1
tryCommand ::
  ( HasCommands env,
    MonadIO m,
    MonadReader env m,
    MonadTVar m
  ) =>
  Command ->
  m (Maybe Stderr)
tryCommand cmd = do
  res <- liftIO $ tryShExitCode cmd

  completedCmds <- asks getCompletedCmds
  modifyTVarM' completedCmds (cmd <|)

  pure $ res ^? _Left

-- | Runs the command, returning the time elapsed along with a possible
-- error.
--
-- @since 0.7
tryCommandLogging ::
  forall m env.
  ( HasCommands env,
    HasLogging env (Region m),
    MonadIORef m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadTBQueue m,
    MonadTime m,
    MonadTVar m,
    MonadUnliftIO m
  ) =>
  Command ->
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
    (rt, Nothing) -> pure $ Right $ U.timeSpecToRelTime rt
    (rt, Just err) -> pure $ Left (U.timeSpecToRelTime rt, err)
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
  ( HasCommands env,
    MonadIORef m,
    MonadReader env m,
    MonadTVar m,
    MonadUnliftIO m
  ) =>
  -- | Function to apply to streamed logs.
  (Log -> m ()) ->
  -- | Command to run.
  Command ->
  -- | Error, if any.
  m (Maybe Stderr)
tryCommandStream logFn cmd@(MkCommand _ cmdTxt) = do
  -- Create pseudo terminal here because otherwise we have trouble streaming
  -- input from child processes. Data gets buffered and trying to override the
  -- buffering strategy (i.e. handles returned by CreatePipe) does not work.
  (recvH, sendH) <- liftIO $ do
    (recvFD, sendFD) <- PTerm.openPseudoTerminal
    recvH <- PBS.fdToHandle recvFD
    sendH <- PBS.fdToHandle sendFD
    Handle.hSetBuffering recvH NoBuffering
    Handle.hSetBuffering sendH NoBuffering
    pure (recvH, sendH)

  -- We use the same pipe for std_out and std_err. The reason is that many
  -- programs will redirect stdout to stderr (e.g. echo ... >&2), and we
  -- will miss this if we don't check. Because this "collapses" stdout
  -- and stderr to the same file descriptor, there isn't much of a reason to
  -- use two different handles.
  let pr =
        (P.shell (T.unpack cmdTxt))
          { std_out = UseHandle sendH,
            std_in = Inherit,
            std_err = UseHandle sendH,
            cwd = Nothing,
            -- We are possibly trying to read from these after the process
            -- closes (e.g. an error), so it is important they are not
            -- closed automatically!
            close_fds = False
          }

  (exitCode, lastRead) <- do
    withRunInIO $ \run ->
      P.withCreateProcess pr $ \_ _ _ ph ->
        run $ streamOutput logFn cmd recvH ph

  completedCmds <- asks getCompletedCmds
  modifyTVarM' completedCmds (cmd <|)

  result <- case exitCode of
    ExitSuccess -> pure Nothing
    ExitFailure _ -> do
      -- Attempt a final read in case there is more data.
      remainingData <- readHandle recvH
      -- Take the most recent valid read of either the lastRead when running
      -- the process, or this final remainingData just attempted. The
      -- semigroup instance favors a successful read, otherwise we take the
      -- left.
      let lastData = case lastRead of
            Nothing -> remainingData
            Just r -> remainingData <> r

      pure $ Just $ readHandleResultToStderr lastData
  liftIO $ do
    Handle.hClose sendH
    Handle.hClose recvH
  pure result

streamOutput ::
  ( MonadIO m,
    MonadIORef m
  ) =>
  -- | Function to apply to streamed logs.
  (Log -> m ()) ->
  -- | Command to run.
  Command ->
  -- | Handle from which to read.
  Handle ->
  -- | Process handle.
  ProcessHandle ->
  -- | Exit code along w/ any leftover data.
  m (Tuple2 ExitCode (Maybe ReadHandleResult))
streamOutput logFn cmd recvH ph = do
  lastReadRef <- newIORef Nothing
  exitCode <- U.untilJust $ do
    result <- readHandle recvH
    case result of
      ReadErr _ ->
        -- We occasionally get invalid reads here -- usually when the command
        -- exits -- likely due to a race condition. It would be nice to
        -- prevent these entirely, but for now ignore them, as it does not
        -- appear that we ever lose important messages.
        pure ()
      ReadSuccess out -> do
        writeIORef lastReadRef (Just (ReadSuccess out))
        let log =
              MkLog
                { cmd = Just cmd,
                  msg = out,
                  lvl = LevelSubCommand,
                  mode = LogModeSet
                }

        logFn log
      ReadNoData -> pure ()
    liftIO $ P.getProcessExitCode ph
  lastRead <- readIORef lastReadRef
  pure (exitCode, lastRead)
