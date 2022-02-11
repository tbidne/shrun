-- | Provides the low-level `IO` functions for running shell commands.
module ShellRun.IO
  ( sh,
    sh_,
    shExitCode,
    tryShExitCode,
    tryTimeSh,
    tryTimeShRegion,
  )
where

import Control.Concurrent qualified as Concurrent
import Control.Exception.Safe (SomeException)
import Control.Exception.Safe qualified as SafeEx
import Control.Monad.Loops qualified as Loops
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef qualified as Ref
import Data.Text qualified as T
import Data.Text.Conversions qualified as TConvert
import GHC.IO.Handle (BufferMode (..), Handle)
import GHC.IO.Handle qualified as Handle
import Refined (NonNegative, Refined)
import ShellRun.Data.Command (Command (..))
import ShellRun.Data.Env (CommandDisplay (..))
import ShellRun.Data.IO (Stderr (..), Stdout (..))
import ShellRun.Logging.Log (Log (..), LogLevel (..), LogMode (..))
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import ShellRun.Prelude
import ShellRun.Utils qualified as Utils
import System.Clock (Clock (..))
import System.Clock qualified as C
import System.Console.Regions (RegionLayout (..))
import System.Console.Regions qualified as Regions
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
  IO (Either (Refined NonNegative Int, Stderr) (Refined NonNegative Int))
tryTimeSh commandDisplay cmd = do
  start <- C.getTime Monotonic
  res <- tryShExitCode commandDisplay cmd Nothing
  end <- C.getTime Monotonic
  let diff = Utils.diffTime start end
  pure $ bimap (diff,) (const diff) res

-- | Version of 'tryTimeSh' that attempts to read the command's
-- @stdout@ + @stderr@.
tryTimeShRegion ::
  CommandDisplay ->
  Command ->
  IO (Either (Refined NonNegative Int, Stderr) (Refined NonNegative Int))
tryTimeShRegion commandDisplay cmd@(MkCommand _ cmdTxt) = do
  Regions.withConsoleRegion Linear $ \region -> do
    lastMessage <- Ref.newIORef ""
    firstRun <- Ref.newIORef True

    -- Create pseudo terminal here because otherwise we have trouble streaming
    -- input from child processes. Data gets buffered and trying to override the
    -- buffering strategy (i.e. handles returned by CreatePipe) does not work.
    (recvFD, sendFD) <- PTerm.openPseudoTerminal
    recvH <- PBS.fdToHandle recvFD
    sendH <- PBS.fdToHandle sendFD
    Handle.hSetBuffering recvH NoBuffering
    Handle.hSetBuffering sendH NoBuffering

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
              close_fds = True
            }

    start <- C.getTime Monotonic
    exitCode <- P.withCreateProcess pr $ \_ _ _ ph -> Loops.untilJust $ do
      -- Delay our first loop by 0.1 seconds. This helps handle showing immediate
      -- errors, as we ensure the process has time to write the err to the socket.
      shouldDelay <- Ref.readIORef firstRun
      if shouldDelay
        then Concurrent.threadDelay 100_000
        else pure ()

      result <- readHandle commandDisplay cmd recvH
      case result of
        ReadErr err -> do
          Ref.writeIORef lastMessage err
          putRegionLog region $ MkLog err Error Set
        ReadSuccess out -> do
          Ref.writeIORef lastMessage out
          putRegionLog region $ MkLog out SubCommand Set
        ReadNoData ->
          pure ()
      P.getProcessExitCode ph

    result <- case exitCode of
      ExitSuccess -> pure $ Right ()
      ExitFailure _ -> do
        msg <- Ref.readIORef lastMessage
        pure $ Left $ MkStderr msg
    end <- C.getTime Monotonic
    let diff = Utils.diffTime start end
        finalResult = bimap (diff,) (const diff) result
    pure finalResult

data ReadHandleResult
  = ReadErr Text
  | ReadSuccess Text
  | ReadNoData
  deriving (Show)

readHandle :: CommandDisplay -> Command -> Handle -> IO ReadHandleResult
readHandle commandDisplay cmd handle = do
  isClosed <- Handle.hIsClosed handle
  canRead <- Handle.hIsReadable handle
  if
      | isClosed ->
          pure $ ReadErr $ displayEx @(List Char) "Handle closed" ""
      | not canRead ->
          pure $ ReadErr $ displayEx @(List Char) "Cannot read from handle" ""
      | otherwise -> do
          output :: Either SomeException ByteString <-
            SafeEx.try $ BS.hGetNonBlocking handle blockSize
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
