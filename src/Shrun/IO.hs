{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the low-level `IO` functions for running shell commands.
--
-- @since 0.1
module Shrun.IO
  ( -- * Stdout/stderr newtypes
    Stdout (..),
    Stderr (..),

    -- * MonadTime shell programs
    tryCommand,
    tryCommandStreamRegion,
    tryCommandStreamNoRegion,
  )
where

import Control.Monad.Loops qualified as Loops
import Data.ByteString qualified as BS
import Data.Sequence ((<|))
import Data.Text qualified as T
import Effects.MonadFs (decodeUtf8Lenient)
import Effects.MonadTime (MonadTime (..))
import GHC.IO.Handle (BufferMode (..))
import GHC.IO.Handle qualified as Handle
import Shrun.Configuration.Env.Types (HasCompletedCmds (..), HasLogging (..))
import Shrun.Data.Command (Command (..))
import Shrun.Data.Supremum (Supremum (..))
import Shrun.Logging.Log qualified as Log
import Shrun.Logging.RegionLogger (RegionLogger (..))
import Shrun.Logging.Types
  ( Log (..),
    LogDest (..),
    LogLevel (..),
    LogMode (..),
  )
import Shrun.Prelude
import System.Console.Regions (ConsoleRegion, RegionLayout (..))
import System.Console.Regions qualified as Regions
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

-- | @since 0.5
makePrisms ''Stdout

-- | Newtype wrapper for stderr.
--
-- @since 0.1
newtype Stderr = MkStderr
  { -- | @since 0.1
    getStderr :: Text
  }

-- | @since 0.5
makePrisms ''Stderr

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
        view _MkStderr
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

-- | Version of 'sh' that returns ('ExitCode', 'Stdout', 'Stderr')
--
-- @since 0.1
shExitCode :: Command -> Maybe FilePath -> IO (ExitCode, Stdout, Stderr)
shExitCode (MkCommand _ cmd) path = do
  (exitCode, stdout, stderr) <- P.readCreateProcessWithExitCode proc ""
  pure (exitCode, wrap MkStdout stdout, wrap MkStderr stderr)
  where
    proc = (P.shell (T.unpack cmd)) {P.cwd = path}
    wrap f = f . T.strip . T.pack
{-# INLINEABLE shExitCode #-}

-- | Version of 'shExitCode' that returns 'Left' 'Stderr' if there is a failure,
-- 'Right' 'Stdout' otherwise.
--
-- @since 0.1
tryShExitCode :: Command -> Maybe FilePath -> IO (Either Stderr Stdout)
tryShExitCode cmd path = do
  (code, stdout, MkStderr err) <- shExitCode cmd path
  pure $ case code of
    ExitSuccess -> Right stdout
    ExitFailure _ -> Left $ makeStdErr err
{-# INLINEABLE tryShExitCode #-}

-- | Version of 'tryShExitCode' with MonadTime. On success, stdout is not
-- returned.
--
-- @since 0.1
tryCommand ::
  ( HasCompletedCmds env,
    MonadIO m,
    MonadReader env m,
    MonadTVar m
  ) =>
  Command ->
  m (Maybe Stderr)
tryCommand cmd = do
  res <- liftIO $ tryShExitCode cmd Nothing

  completedCmds <- asks getCompletedCmds
  modifyTVarM' completedCmds (cmd <|)

  pure $ res ^? _Left
{-# INLINEABLE tryCommand #-}

-- | Similar to 'tryCommand' except we attempt to stream the commands' output
-- to a 'ConsoleRegion' instead of the usual swallowing.
--
-- @since 0.1
tryCommandStreamRegion ::
  ( HasCompletedCmds env,
    HasLogging env,
    MonadIORef m,
    MonadMask m,
    MonadReader env m,
    MonadTBQueue m,
    MonadTVar m,
    MonadUnliftIO m,
    RegionLogger m,
    Region m ~ ConsoleRegion,
    MonadTime m
  ) =>
  Command ->
  m (Maybe Stderr)
tryCommandStreamRegion cmd = Regions.withConsoleRegion Linear $ \region ->
  tryCommandAnyRegion (Just region) cmd
{-# INLINEABLE tryCommandStreamRegion #-}

-- | We stream the commands' output like 'tryCommandStreamRegion' except we do
-- __not__ create a console region. This function is intended for when we want
-- to send command logs to a file, but do not want to stream them to the
-- console.
--
-- @since 0.1
tryCommandStreamNoRegion ::
  ( HasCompletedCmds env,
    HasLogging env,
    MonadIORef m,
    MonadReader env m,
    MonadTBQueue m,
    MonadTVar m,
    MonadUnliftIO m,
    RegionLogger m,
    Region m ~ ConsoleRegion,
    MonadTime m
  ) =>
  Command ->
  m (Maybe Stderr)
tryCommandStreamNoRegion = tryCommandAnyRegion Nothing
{-# INLINEABLE tryCommandStreamNoRegion #-}

-- | Similar to 'tryCommand' except we attempt to stream the commands' output
-- to a 'ConsoleRegion' instead of the usual swallowing.
--
-- @since 0.1
tryCommandAnyRegion ::
  ( HasCompletedCmds env,
    HasLogging env,
    MonadIORef m,
    MonadReader env m,
    MonadTBQueue m,
    MonadTVar m,
    MonadUnliftIO m,
    RegionLogger m,
    Region m ~ ConsoleRegion,
    MonadTime m
  ) =>
  Maybe ConsoleRegion ->
  Command ->
  m (Maybe Stderr)
tryCommandAnyRegion mRegion cmd@(MkCommand _ cmdTxt) = do
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
  -- will miss this if we don't check b Because this "collapses" stdout
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
        run $ streamOutput mRegion cmd recvH ph

  completedCmds <- asks getCompletedCmds
  modifyTVarM' completedCmds (cmd <|)

  result <- case exitCode of
    ExitSuccess -> pure $ Right ()
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

      pure $ Left $ readHandleResultToStderr lastData
  liftIO $ do
    Handle.hClose sendH
    Handle.hClose recvH
  pure $ result ^? _Left
{-# INLINEABLE tryCommandAnyRegion #-}

streamOutput ::
  ( HasLogging env,
    MonadIO m,
    MonadIORef m,
    MonadReader env m,
    MonadTBQueue m,
    RegionLogger m,
    Region m ~ ConsoleRegion,
    MonadTime m
  ) =>
  Maybe ConsoleRegion ->
  Command ->
  Handle ->
  ProcessHandle ->
  m (Tuple2 ExitCode (Maybe ReadHandleResult))
streamOutput mRegion cmd recvH ph = do
  lastReadRef <- newIORef Nothing
  exitCode <- Loops.untilJust $ do
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
        cmdLogging <- asks getCmdLogging
        let logDest =
              if cmdLogging
                then LogBoth
                else LogFile
            log =
              MkLog
                { cmd = Just cmd,
                  msg = out,
                  lvl = SubCommand,
                  mode = Set,
                  dest = logDest
                }
        case mRegion of
          Nothing -> Log.putLog log
          Just region -> Log.putRegionLog region log
      ReadNoData -> pure ()
    liftIO $ P.getProcessExitCode ph
  lastRead <- readIORef lastReadRef
  pure (exitCode, lastRead)
{-# INLINEABLE streamOutput #-}
