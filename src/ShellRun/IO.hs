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
import GHC.IO.Handle qualified as Handle
import ShellRun.Class.MonadLogger (LogLevel (..), LogMode (..))
import ShellRun.Class.MonadLogger qualified as ML
import ShellRun.Types.Command (Command (..))
import ShellRun.Types.IO (Stderr (..), Stdout (..))
import ShellRun.Types.NonNegative (NonNegative (..))
import ShellRun.Utils qualified as Utils
import System.Clock qualified as C
import System.Exit (ExitCode (..))
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
tryShExitCode command@(MkCommand cmd) path = do
  (code, stdout, MkStderr err) <- shExitCode command path
  pure $ case code of
    ExitSuccess -> Right stdout
    ExitFailure _ ->
      Left $
        MkStderr $
          "Error running `"
            <> cmd
            <> "`: "
            <> err

-- | Version of 'tryShExitCode' that also returns (t, stdout/stderr), where
-- /t/ is the time the command took in seconds.
tryTimeSh ::
  Command ->
  Maybe FilePath ->
  IO (Either (NonNegative, Stderr) NonNegative)
tryTimeSh cmd path = do
  start <- C.getTime C.Monotonic
  res <- tryShExitCode cmd path
  end <- C.getTime C.Monotonic
  let diff = Utils.diffTime start end
  pure $ Bifunctor.bimap (diff,) (const diff) res

tryTimeShWithStdout ::
  Command ->
  Maybe FilePath ->
  IO (Either (NonNegative, Stderr) NonNegative)
tryTimeShWithStdout (MkCommand cmd) path = do
  start <- C.getTime C.Monotonic
  ec <- P.withCreateProcess pr $ \stdin stdout stderr ph -> do
    Utils.whileNothing (P.getProcessExitCode ph) $ do
      case (stdin, stdout, stderr) of
        (_, Just hOut, _) -> do
          out :: Either IOException String <- Except.try $ Handle.hGetLine hOut
          case out of
            Left _ -> pure ()
            Right x -> do
              ML.clearNoLine
              ML.logLevelMode Info Line $ cmd <> ": " <> T.pack x
        _ -> pure ()
  end <- C.getTime C.Monotonic
  let diff = Utils.diffTime start end

  case ec of
    ExitSuccess -> pure $ Right diff
    ExitFailure _ -> pure $ Left (diff, MkStderr "failed")
  where
    pr =
      (P.shell (T.unpack cmd))
        { std_out = CreatePipe,
          std_in = CreatePipe,
          cwd = path
        }