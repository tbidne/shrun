-- | Provides the low-level `IO` functions for running shell commands.
--
-- @since 0.1.0.0
module ShellRun.IO
  ( -- * Running shell programs
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
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import GHC.IO.Handle (Handle)
import GHC.IO.Handle qualified as Handle
import ShellRun.Data.Command (Command (..))
import ShellRun.Data.Env (CommandDisplay (..))
import ShellRun.Data.IO (Stderr (..), Stdout (..))
import ShellRun.Data.Supremum (Supremum (..))
import ShellRun.Prelude
import ShellRun.Utils qualified as Utils
import System.Exit (ExitCode (..))
import System.Process qualified as P

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
          let outDecoded = fmap Utils.decodeUtf8Lenient output
          pure $ case outDecoded of
            Left ex -> ReadErr $ readEx ex
            Right "" -> ReadNoData
            Right o -> ReadSuccess $ name <> ": " <> stripChars o
  where
    name = Utils.displayCommand commandDisplay cmd
    displayEx :: Show a => Text -> a -> Text
    displayEx prefix =
      getStderr
        . makeStdErr commandDisplay cmd
        . (<>) prefix
        . showt
    readEx = displayEx "IOException reading handle: "

makeStdErr :: CommandDisplay -> Command -> Text -> Stderr
makeStdErr commandDisplay cmd err =
  MkStderr $
    "Error running '"
      <> name
      <> "': "
      <> stripChars err
  where
    name = Utils.displayCommand commandDisplay cmd

stripChars :: Text -> Text
stripChars = T.stripEnd . T.replace "\r" ""

blockSize :: Int
blockSize = 1024
