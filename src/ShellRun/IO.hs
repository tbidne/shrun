{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun.IO
  ( sh,
    sh_,
    shExitCode,
    tryShExitCode,
    tryTimeSh,
    tryTimeShWithStdout,
  )
where

import Control.Exception (IOException)
import Control.Exception qualified as Except
import Data.Bifunctor qualified as Bifunctor
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.IO.Handle (Handle)
import GHC.IO.Handle qualified as Handle
import ShellRun.Class.MonadLogger (LogLevel (..), LogMode (..))
import ShellRun.Class.MonadLogger qualified as ML
import ShellRun.Math (NonNegative (..))
import ShellRun.Types.Command (Command (..))
import ShellRun.Types.IO (Stderr (..), Stdout (..))
import ShellRun.Utils qualified as Utils
import System.Clock (Clock (..))
import System.Clock qualified as C
import System.Exit (ExitCode (..))
import System.IO.Strict qualified as StrictIO
import System.Process (CreateProcess (..), StdStream (..))
import System.Process qualified as P

-- | Returns the result of running a shell command given by
-- 'Text' on 'FilePath'.
sh :: Command -> Maybe FilePath -> IO Text
sh (MkCommand cmd) fp = T.pack <$> P.readCreateProcess proc ""
  where
    proc = (P.shell (T.unpack cmd)) {P.cwd = fp}

-- | Version of 'sh' that ignores the return value.
sh_ :: T.Text -> Maybe FilePath -> IO ()
sh_ cmd fp = P.readCreateProcess proc "" $> ()
  where
    proc = (P.shell (T.unpack cmd)) {P.cwd = fp}

-- | Version of 'sh' that returns ('ExitCode', stdout, stderr)
shExitCode :: Command -> Maybe FilePath -> IO (ExitCode, Stdout, Stderr)
shExitCode (MkCommand cmd) path = do
  (exitCode, stdout, stderr) <- P.readCreateProcessWithExitCode proc ""
  pure (exitCode, wrap MkStdout stdout, wrap MkStderr stderr)
  where
    proc = (P.shell (T.unpack cmd)) {P.cwd = path}
    wrap f = f . T.strip . T.pack

-- | Returns 'Left' stderr if there is a failure, 'Right' stdout otherwise.
tryShExitCode :: Command -> Maybe FilePath -> IO (Either Stderr Stdout)
tryShExitCode command path = do
  (code, stdout, MkStderr err) <- shExitCode command path
  pure $ case code of
    ExitSuccess -> Right stdout
    ExitFailure _ -> Left $ makeStdErr command err

-- | Version of 'tryShExitCode' that also returns (t, stdout/stderr), where
-- /t/ is the time the command took in seconds.
tryTimeSh ::
  Command ->
  Maybe FilePath ->
  IO (Either (NonNegative, Stderr) NonNegative)
tryTimeSh cmd path = do
  start <- C.getTime Monotonic
  res <- tryShExitCode cmd path
  end <- C.getTime Monotonic
  let diff = Utils.diffTime start end
  pure $ Bifunctor.bimap (diff,) (const diff) res

tryTimeShWithStdout ::
  Command ->
  Maybe FilePath ->
  IO (Either (NonNegative, Stderr) NonNegative)
tryTimeShWithStdout command@(MkCommand cmd) path = do
  start <- C.getTime Monotonic
  result <- P.withCreateProcess pr $ \_ maybeHStdout maybeHStderr ph -> do
    exitCode <- Utils.whileNothing (P.getProcessExitCode ph) $ do
      case maybeHStdout of
        Just hOut -> do
          out :: Either IOException String <- Except.try $ Handle.hGetLine hOut
          case out of
            Left _ -> pure ()
            Right x -> do
              ML.clearNoLine
              ML.logLevelMode Info Line $ cmd <> ": " <> T.pack x
        _ -> pure ()

    case exitCode of
      ExitSuccess -> pure $ Right ()
      ExitFailure _ -> do
        err <- handleToStderr command maybeHStderr
        pure $ Left err

  end <- C.getTime Monotonic
  let diff = Utils.diffTime start end
      finalResult = Bifunctor.bimap (diff,) (const diff) result

  pure finalResult
  where
    pr =
      (P.shell (T.unpack cmd))
        { std_out = CreatePipe,
          std_in = CreatePipe,
          std_err = CreatePipe,
          cwd = path
        }

handleToStderr :: Command -> Maybe Handle -> IO Stderr
handleToStderr command = \case
  Nothing -> pure $ makeStdErr command noHandle
  Just hErr -> do
    -- NOTE: Using `StrictIO` here as the one in base is lazy, which causes an
    -- error where we end up reading after the FD is closed. This can be "fixed"
    -- by printing the output first, but adding a superfluous print statement
    -- to force the read is suboptimal. For now, StrictIO seems to solve this
    -- problem, though we may be able to remove the dependency in favor
    -- of Handle.hGetContents' once we can upgrade to base 4.15.0.0.
    errStr :: Either IOException String <-
      Except.try $
        StrictIO.run $ StrictIO.hGetContents hErr
    pure $ case errStr of
      Left ex -> makeStdErr command $ readHErr ex
      Right err -> makeStdErr command $ T.pack err
  where
    noHandle = "No handle from which to read stderr"
    readHErr = (<>) "IOException reading stderr: " . T.pack . show

makeStdErr :: Command -> Text -> Stderr
makeStdErr (MkCommand cmd) err =
  MkStderr $
    "Error running `"
      <> cmd
      <> "`: "
      <> err
