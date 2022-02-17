-- | Provides the low-level `IO` functions for running shell commands.
--
-- @since 0.1.0.0
module ShellRun.IO
  ( -- * Stdout/stderr newtypes
    Stdout (..),
    Stderr (..),

    -- * Timing shell programs
    tryTimeSh,
    tryTimeShRegion,

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
import ShellRun.Env
  ( CommandDisplay (..),
    HasCommandDisplay (..),
    HasCommandTruncation (..),
  )
import ShellRun.Env qualified as Env
import ShellRun.Logging.Log (Log (..), LogLevel (..), LogMode (..))
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
tryShExitCode :: CommandDisplay -> Command -> Maybe FilePath -> IO (Either Stderr Stdout)
tryShExitCode commandDisplay cmd path = do
  (code, stdout, MkStderr err) <- shExitCode cmd path
  pure $ case code of
    ExitSuccess -> Right stdout
    ExitFailure _ -> Left $ makeStdErr commandDisplay cmd err

tryTimeSh ::
  ( HasCommandDisplay env,
    MonadIO m,
    MonadReader env m
  ) =>
  Command ->
  m (Either (Tuple2 RNonNegative Stderr) RNonNegative)
tryTimeSh cmd = do
  commandDisplay <- asks getCommandDisplay
  liftIO $ do
    start <- C.getTime Monotonic
    res <- tryShExitCode commandDisplay cmd Nothing
    end <- C.getTime Monotonic
    let diff = Utils.diffTime start end
    pure $ bimap (diff,) (const diff) res

tryTimeShRegion ::
  ( HasCommandDisplay env,
    HasCommandTruncation env,
    MonadIO m,
    MonadMask m,
    MonadReader env m,
    MonadUnliftIO m,
    RegionLogger m,
    Region m ~ ConsoleRegion
  ) =>
  Command ->
  m (Either (Tuple2 RNonNegative Stderr) RNonNegative)
tryTimeShRegion cmd@(MkCommand _ cmdTxt) =
  Regions.withConsoleRegion Linear $ \region -> do
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
      P.withCreateProcess pr $ \_ _ _ ph -> runner $ streamOutput region cmd recvH ph
    end <- liftIO $ C.getTime Monotonic

    result <- case exitCode of
      ExitSuccess -> pure $ Right ()
      ExitFailure _ -> do
        -- Attempt a final read in case there is more data.
        remainingData <- readHandle cmd recvH
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
  ( HasCommandDisplay env,
    HasCommandTruncation env,
    MonadIO m,
    MonadReader env m,
    RegionLogger m,
    Region m ~ ConsoleRegion
  ) =>
  ConsoleRegion ->
  Command ->
  Handle ->
  ProcessHandle ->
  m (Tuple2 ExitCode (Maybe ReadHandleResult))
streamOutput region cmd recvH ph = do
  lastReadRef <- liftIO $ IORef.newIORef Nothing
  exitCode <- Loops.untilJust $ do
    result <- readHandle cmd recvH
    case result of
      ReadErr _ -> do
        -- We occasionally get invalid reads here -- usually when the command
        -- exits -- likely due to a race condition. It would be nice to
        -- prevent these entirely, but for now ignore them, as it does not
        -- appear that we ever lose important messages.
        pure ()
      ReadSuccess out -> do
        liftIO $ IORef.writeIORef lastReadRef (Just (ReadSuccess out))
        let log = MkLog out SubCommand Set
        putRegionLog region log
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
readHandle ::
  ( HasCommandDisplay env,
    HasCommandTruncation env,
    MonadIO m,
    MonadReader env m
  ) =>
  Command ->
  Handle ->
  m ReadHandleResult
readHandle cmd handle = do
  commandTruncation <- asks getCommandTruncation
  commandDisplay <- asks getCommandDisplay

  let name = Env.displayCommandTruncation commandTruncation commandDisplay cmd
      displayEx :: Show a => Text -> a -> Text
      displayEx prefix =
        getStderr
          . makeStdErr commandDisplay cmd
          . (<>) prefix
          . showt
      readEx = displayEx "IOException reading handle: "

  isClosed <- liftIO $ Handle.hIsClosed handle
  canRead <- liftIO $ Handle.hIsReadable handle
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
            Right o -> ReadSuccess $ name <> ": " <> stripChars o

makeStdErr :: CommandDisplay -> Command -> Text -> Stderr
makeStdErr commandDisplay cmd err =
  MkStderr $
    "Error running '"
      <> name
      <> "': "
      <> stripChars err
  where
    name = Env.displayCommand commandDisplay cmd

stripChars :: Text -> Text
stripChars = T.stripEnd . T.replace "\r" ""

blockSize :: Int
blockSize = 1024
