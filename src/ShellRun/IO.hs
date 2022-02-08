-- | Provides the low-level `IO` functions for running shell commands.
module ShellRun.IO
  ( sh,
    sh_,
    shExitCode,
    tryShExitCode,
    tryTimeSh,
    tryTimeShCommandOutput,
  )
where

import Control.Concurrent qualified as Concurrent
import Control.Exception (IOException)
import Control.Exception qualified as Except
import Control.Monad.Loops qualified as Loops
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Conversions qualified as TConvert
import GHC.IO.Handle (BufferMode (..), Handle)
import GHC.IO.Handle qualified as Handle
import Refined (NonNegative, Refined)
import ShellRun.Data.Command (Command (..))
import ShellRun.Data.Env (CommandDisplay (..))
import ShellRun.Data.IO (Stderr (..), Stdout (..))
import ShellRun.Logging (LogQueue (..))
import ShellRun.Logging qualified as Logging
import ShellRun.Prelude
import ShellRun.Utils qualified as Utils
import System.Clock (Clock (..))
import System.Clock qualified as C
import System.Exit (ExitCode (..))
import System.IO (FilePath)
import System.Posix.IO.ByteString qualified as PBS
import System.Posix.Terminal qualified as PTerm
import System.Process (CreateProcess (..), StdStream (..))
import System.Process qualified as P

-- | Returns the result of running a shell command given by
-- 'Text' on 'FilePath'.
sh :: Command -> Maybe FilePath -> IO Text
sh (MkCommand _ cmd) fp = T.pack <$> P.readCreateProcess proc ""
  where
    proc = (P.shell (T.unpack cmd)) {P.cwd = fp}

-- | Version of 'sh' that ignores the return value.
sh_ :: Command -> Maybe FilePath -> IO ()
sh_ cmd = ($> ()) . sh cmd

-- | Version of 'sh' that returns ('ExitCode', 'Stdout', 'Stderr')
shExitCode :: Command -> Maybe FilePath -> IO (ExitCode, Stdout, Stderr)
shExitCode (MkCommand _ cmd) path = do
  (exitCode, stdout, stderr) <- P.readCreateProcessWithExitCode proc ""
  pure (exitCode, wrap MkStdout stdout, wrap MkStderr stderr)
  where
    proc = (P.shell (T.unpack cmd)) {P.cwd = path}
    wrap f = f . T.strip . T.pack

-- | Version of 'shExitCode' that returns 'Left' 'Stderr' if there is a failure,
-- 'Right' 'Stdout' otherwise.
tryShExitCode :: CommandDisplay -> Command -> Maybe FilePath -> IO (Either Stderr Stdout)
tryShExitCode commandDisplay cmd path = do
  (code, stdout, MkStderr err) <- shExitCode cmd path
  pure $ case code of
    ExitSuccess -> Right stdout
    ExitFailure _ -> Left $ makeStdErr commandDisplay cmd err

-- | Version of 'tryShExitCode' that also returns the command's
-- duration. 'Stdout' is not returned on success.
tryTimeSh ::
  CommandDisplay ->
  Command ->
  Maybe FilePath ->
  IO (Either (Refined NonNegative Int, Stderr) (Refined NonNegative Int))
tryTimeSh commandDisplay cmd path = do
  start <- C.getTime Monotonic
  res <- tryShExitCode commandDisplay cmd path
  end <- C.getTime Monotonic
  let diff = Utils.diffTime start end
  pure $ bimap (diff,) (const diff) res

-- | Version of 'tryTimeSh' that attempts to read the command's
-- @stdout@ + @stderr@.
tryTimeShCommandOutput ::
  LogQueue ->
  CommandDisplay ->
  Command ->
  Maybe FilePath ->
  IO (Either (Refined NonNegative Int, Stderr) (Refined NonNegative Int))
tryTimeShCommandOutput logQueue commandDisplay cmd@(MkCommand _ cmdTxt) path = do
  -- Create pseudo terminal here because otherwise we have trouble streaming
  -- input from child processes. Data gets buffered and trying to override the
  -- buffering strategy (i.e. handles returned by CreatePipe) does not work.
  (recvFD, sendFD) <- PTerm.openPseudoTerminal
  recvH <- PBS.fdToHandle recvFD
  sendH <- PBS.fdToHandle sendFD
  Handle.hSetBuffering recvH NoBuffering
  Handle.hSetBuffering sendH NoBuffering

  start <- C.getTime Monotonic

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
            cwd = path,
            close_fds = True
          }
  (_, _, _, ph) <- P.createProcess_ "createProcess_" pr
  exitCode <- Loops.untilJust $ do
    isOpen <- Handle.hIsOpen recvH
    canRead <- Handle.hIsReadable recvH
    maybeLog <-
      if
          | not isOpen -> do
            let (MkStderr err) = makeStdErr commandDisplay cmd "Handle not open"
            pure $ Just $ Logging.logError err
          | not canRead -> do
            let (MkStderr err) = makeStdErr commandDisplay cmd "Handle not readable"
            pure $ Just $ Logging.logError err
          | otherwise -> do
            result <- readHandle commandDisplay cmd recvH
            case result of
              ReadErr err ->
                pure $ Just $ Logging.logError err
              ReadSuccess out ->
                pure $ Just $ Logging.logSubCommand out
              ReadNoData -> pure Nothing

    case maybeLog of
      Just l -> Logging.writeQueue logQueue l
      Nothing -> pure ()

    -- Sleep for a 0.1 seconds. This is helpful for avoiding spamming the logs
    -- with duplicate errors. For instance, when a command finishes, there will
    -- be a race between when the handle recvH can no longer be read (but still
    -- reports that it can be i.e. hIsReadable), and when getProcessExitCode
    -- returns (Just ExitCode). If we let the loop execute quickly, we can see
    -- this error many times before exit. The delay is a bit of a hack, but it
    -- mitigates seeing the same error many times, and additionally usually
    -- prevents the aforementioned invalid read altogether.
    Concurrent.threadDelay 100_000
    P.getProcessExitCode ph

  result <- case exitCode of
    ExitSuccess -> pure $ Right ()
    ExitFailure _ -> do
      err <- readHandle commandDisplay cmd recvH
      pure $ Left $ readResultToStdErr err

  end <- C.getTime Monotonic
  let diff = Utils.diffTime start end
      finalResult = bimap (diff,) (const diff) result

  pure finalResult

data ReadHandleResult
  = ReadErr Text
  | ReadSuccess Text
  | ReadNoData

readResultToStdErr :: ReadHandleResult -> Stderr
readResultToStdErr (ReadErr e) = MkStderr e
readResultToStdErr (ReadSuccess o) = MkStderr o
readResultToStdErr ReadNoData = MkStderr "(No data in handle, see above logs)"

readHandle :: CommandDisplay -> Command -> Handle -> IO ReadHandleResult
readHandle commandDisplay cmd handle = do
  output :: Either IOException ByteString <-
    Except.try $ BS.hGetNonBlocking handle blockSize
  let outDecoded = fmap (TConvert.decodeConvertText . TConvert.UTF8) output
  pure $ case outDecoded of
    Left ex -> ReadErr $ readEx ex
    Right Nothing -> ReadErr $ utf8Err outDecoded
    Right (Just "") -> ReadNoData
    Right (Just o) -> ReadSuccess $ name <> ": " <> stripChars (T.pack o)
  where
    name = Utils.displayCommand commandDisplay cmd
    displayEx :: Show a => Text -> a -> Text
    displayEx prefix =
      getStderr
        . makeStdErr commandDisplay cmd
        . (<>) prefix
        . showt
    readEx = displayEx "IOException reading handle: "
    utf8Err = displayEx "Could not decode UTF-8: "

makeStdErr :: CommandDisplay -> Command -> Text -> Stderr
makeStdErr commandDisplay cmd err =
  MkStderr $
    "Error running `"
      <> name
      <> "`: "
      <> stripChars err
  where
    name = Utils.displayCommand commandDisplay cmd

stripChars :: Text -> Text
stripChars = T.stripEnd . T.replace "\r" ""

blockSize :: Int
blockSize = 1024
