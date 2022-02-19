-- | Provides the low-level `IO` functions for running shell commands.
--
-- @since 0.1.0.0
module ShellRun.IO
  ( -- * Stdout/stderr newtypes
    Stdout (..),
    Stderr (..),

    -- * Timing shell programs
    tryTimeSh,
    tryTimeShStreamNoRegion,
    tryTimeShStreamRegion,

    -- * Low level running shell programs
    sh,
    sh_,
    shExitCode,
    tryShExitCode,

    -- * File Handles
    ReadHandleResult (..),
    readHandle,
    readHandleResultToStderr,
  )
where

import Control.Exception.Safe (SomeException)
import Control.Exception.Safe qualified as SafeEx
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.IO.Unlift qualified as UAsync
import Control.Monad.Loops qualified as Loops
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef qualified as IORef
import Data.Text qualified as T
import GHC.IO.Handle (BufferMode (..), Handle)
import GHC.IO.Handle qualified as Handle
import ShellRun.Command (Command (..))
import ShellRun.Data.Supremum (Supremum (..))
import ShellRun.Env.Types (CommandLogging (..), HasCommandLogging (..))
import ShellRun.Logging.Log (Log (..), LogDest (..), LogLevel (..), LogMode (..))
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import ShellRun.Prelude
import ShellRun.Utils qualified as Utils
import System.Clock (Clock (..))
import System.Clock qualified as C
import System.Console.Regions (ConsoleRegion, RegionLayout (..))
import System.Console.Regions qualified as Regions
import System.Exit (ExitCode (..))
import System.Posix.IO.ByteString qualified as PBS
import System.Posix.Terminal qualified as PTerm
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..))
import System.Process qualified as P

-- | Newtype wrapper for stdout.
--
-- @since 0.1.0.0
newtype Stdout = MkStdout
  { -- | @since 0.1.0.0
    getStdout :: Text
  }

-- | Newtype wrapper for stderr.
--
-- @since 0.1.0.0
newtype Stderr = MkStderr
  { -- | @since 0.1.0.0
    getStderr :: Text
  }

-- | Returns the result of running a shell command given by
-- 'Text' on 'FilePath'.
--
-- @since 0.1.0.0
sh :: Command -> Maybe FilePath -> IO Text
sh (MkCommand _ cmd) fp = T.pack <$> P.readCreateProcess proc ""
  where
    proc = (P.shell (T.unpack cmd)) {P.cwd = fp}

-- | Version of 'sh' that ignores the return value.
--
-- @since 0.1.0.0
sh_ :: Command -> Maybe FilePath -> IO ()
sh_ cmd = void . sh cmd

-- | Version of 'sh' that returns ('ExitCode', 'Stdout', 'Stderr')
--
-- @since 0.1.0.0
shExitCode :: Command -> Maybe FilePath -> IO (ExitCode, Stdout, Stderr)
shExitCode (MkCommand _ cmd) path = do
  (exitCode, stdout, stderr) <- P.readCreateProcessWithExitCode proc ""
  pure (exitCode, wrap MkStdout stdout, wrap MkStderr stderr)
  where
    proc = (P.shell (T.unpack cmd)) {P.cwd = path}
    wrap f = f . T.strip . T.pack

-- | Version of 'shExitCode' that returns 'Left' 'Stderr' if there is a failure,
-- 'Right' 'Stdout' otherwise.
--
-- @since 0.1.0.0
tryShExitCode :: Command -> Maybe FilePath -> IO (Either Stderr Stdout)
tryShExitCode cmd path = do
  (code, stdout, MkStderr err) <- shExitCode cmd path
  pure $ case code of
    ExitSuccess -> Right stdout
    ExitFailure _ -> Left $ makeStdErr err

-- | Version of 'tryShExitCode' with timing. On success, stdout is not
-- returned.
--
-- @since 0.1.0.0
tryTimeSh :: Command -> IO (Either (Tuple2 Natural Stderr) Natural)
tryTimeSh cmd = do
  start <- C.getTime Monotonic
  res <- tryShExitCode cmd Nothing
  end <- C.getTime Monotonic
  let diff = Utils.diffTime start end
  pure $ bimap (diff,) (const diff) res

-- | Similar to 'tryTimeSh' except we attempt to stream the commands' output
-- to a 'ConsoleRegion' instead of the usual swallowing.
--
-- @since 0.1.0.0
tryTimeShStreamRegion ::
  ( HasCommandLogging env,
    MonadMask m,
    MonadReader env m,
    MonadUnliftIO m,
    RegionLogger m,
    Region m ~ ConsoleRegion
  ) =>
  Command ->
  m (Either (Tuple2 Natural Stderr) Natural)
tryTimeShStreamRegion cmd = Regions.withConsoleRegion Linear $ \region ->
  tryTimeShAnyRegion (Just region) cmd

-- | We stream the commands' output like 'tryTimeShStreamRegion' except we do
-- __not__ create a console region. This function is intended for when we want
-- to send command logs to a file, but do not want to stream them to the
-- console.
--
-- @since 0.1.0.0
tryTimeShStreamNoRegion ::
  ( HasCommandLogging env,
    MonadReader env m,
    MonadUnliftIO m,
    RegionLogger m,
    Region m ~ ConsoleRegion
  ) =>
  Command ->
  m (Either (Tuple2 Natural Stderr) Natural)
tryTimeShStreamNoRegion = tryTimeShAnyRegion Nothing

-- | Similar to 'tryTimeSh' except we attempt to stream the commands' output
-- to a 'ConsoleRegion' instead of the usual swallowing.
--
-- @since 0.1.0.0
tryTimeShAnyRegion ::
  ( HasCommandLogging env,
    MonadIO m,
    MonadReader env m,
    MonadUnliftIO m,
    RegionLogger m,
    Region m ~ ConsoleRegion
  ) =>
  Maybe ConsoleRegion ->
  Command ->
  m (Either (Tuple2 Natural Stderr) Natural)
tryTimeShAnyRegion mRegion cmd@(MkCommand _ cmdTxt) = do
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
  -- will miss this if we don't check both. Because this "collapses" stdout
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

  start <- liftIO $ C.getTime Monotonic
  (exitCode, lastRead) <- UAsync.withRunInIO $ \runner ->
    P.withCreateProcess pr $ \_ _ _ ph -> runner $ streamOutput mRegion cmd recvH ph
  end <- liftIO $ C.getTime Monotonic

  result <- case exitCode of
    ExitSuccess -> pure $ Right ()
    ExitFailure _ -> do
      -- Attempt a final read in case there is more data.
      remainingData <- liftIO $ readHandle recvH
      -- Take the most recent valid read of either the lastRead when running
      -- the process, or this final remainingData just attempted. The
      -- semigroup instance favors a successful read, otherwise we take the
      -- left.
      let lastData = case lastRead of
            Nothing -> remainingData
            Just r -> remainingData <> r

      pure $ Left $ readHandleResultToStderr lastData
  liftIO $ do
    Handle.hClose sendH
    Handle.hClose recvH
  let diff = Utils.diffTime start end
      finalResult = bimap (diff,) (const diff) result
  pure finalResult

streamOutput ::
  ( HasCommandLogging env,
    MonadIO m,
    MonadReader env m,
    RegionLogger m,
    Region m ~ ConsoleRegion
  ) =>
  Maybe ConsoleRegion ->
  Command ->
  Handle ->
  ProcessHandle ->
  m (Tuple2 ExitCode (Maybe ReadHandleResult))
streamOutput mRegion cmd recvH ph = do
  lastReadRef <- liftIO $ IORef.newIORef Nothing
  exitCode <- Loops.untilJust $ do
    result <- liftIO $ readHandle recvH
    case result of
      ReadErr _ ->
        -- We occasionally get invalid reads here -- usually when the command
        -- exits -- likely due to a race condition. It would be nice to
        -- prevent these entirely, but for now ignore them, as it does not
        -- appear that we ever lose important messages.
        pure ()
      ReadSuccess out -> do
        liftIO $ IORef.writeIORef lastReadRef (Just (ReadSuccess out))
        commandLogging <- asks getCommandLogging
        let logDest = case commandLogging of
              Disabled -> LogFile
              Enabled -> LogBoth
            log =
              MkLog
                { cmd = Just cmd,
                  msg = out,
                  lvl = SubCommand,
                  mode = Set,
                  dest = logDest
                }
        case mRegion of
          Nothing -> putLog log
          Just region -> putRegionLog region log
      ReadNoData -> pure ()
    liftIO $ P.getProcessExitCode ph
  lastRead <- liftIO $ IORef.readIORef lastReadRef
  pure (exitCode, lastRead)

-- | Result from reading a handle. The ordering is based on:
--
-- @
-- 'ReadErr' _ < 'ReadNoData' < 'ReadSuccess'
-- @
--
-- The 'Semigroup' instance is based on this ordering, taking the greatest
-- element. For identical constructors, the left argument is taken.
--
-- @since 0.1.0.0
data ReadHandleResult
  = -- | Error encountered while trying to read a handle.
    --
    -- @since 0.1.0.0
    ReadErr Text
  | -- | Successfully read data from the handle.
    --
    -- @since 0.1.0.0
    ReadSuccess Text
  | -- | Successfully read no data from the handle.
    --
    -- @since 0.1.0.0
    ReadNoData
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )
  deriving
    ( -- | @since 0.1.0.0
      Semigroup,
      -- | @since 0.1.0.0
      Monoid
    )
    via Supremum ReadHandleResult

-- | @since 0.1.0.0
instance Bounded ReadHandleResult where
  minBound = ReadErr ""
  maxBound = ReadSuccess ""

-- | @since 0.1.0.0
instance Ord ReadHandleResult where
  compare x y | x == y = EQ
  compare (ReadSuccess _) _ = GT
  compare _ (ReadSuccess _) = LT
  compare ReadNoData _ = GT
  compare _ ReadNoData = LT
  compare (ReadErr _) _ = GT

-- | Turns a 'ReadHandleResult' into a 'Stderr'.
--
-- @since 0.1.0.0
readHandleResultToStderr :: ReadHandleResult -> Stderr
readHandleResultToStderr ReadNoData = MkStderr "<No data>"
readHandleResultToStderr (ReadErr err) = MkStderr err
readHandleResultToStderr (ReadSuccess err) = MkStderr err

-- | Attempts to read from the handle. The parameter 'CommandDisplay' and
-- 'Command' are used in formatting.
--
-- @since 0.1.0.0
readHandle :: Handle -> IO ReadHandleResult
readHandle handle = do
  let displayEx :: Show a => Text -> a -> Text
      displayEx prefix =
        getStderr
          . makeStdErr
          . (<>) prefix
          . showt
      readEx = displayEx "IOException reading handle: "

  isClosed <- Handle.hIsClosed handle
  canRead <- Handle.hIsReadable handle
  if
      | isClosed ->
          pure $ ReadErr $ displayEx @(List Char) "Handle closed" ""
      | not canRead ->
          pure $ ReadErr $ displayEx @(List Char) "Cannot read from handle" ""
      | otherwise -> do
          output :: Either SomeException ByteString <-
            liftIO $ SafeEx.try $ BS.hGetNonBlocking handle blockSize
          let outDecoded = fmap Utils.decodeUtf8Lenient output
          pure $ case outDecoded of
            Left ex -> ReadErr $ readEx ex
            Right "" -> ReadNoData
            Right o -> ReadSuccess $ stripChars o

makeStdErr :: Text -> Stderr
makeStdErr err = MkStderr $ "Error: '" <> stripChars err

stripChars :: Text -> Text
stripChars = T.stripEnd . T.replace "\r" ""

blockSize :: Int
blockSize = 1024
