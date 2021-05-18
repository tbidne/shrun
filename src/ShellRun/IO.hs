module ShellRun.IO
  ( sh,
    sh_,
    shExitCode,
    tryShExitCode,
    tryTimeSh,
  )
where

import Data.Bifunctor qualified as Bifunctor
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as T
import ShellRun.Types.Command (Command (..))
import ShellRun.Types.NonNegative (NonNegative (..))
import ShellRun.Types.IO (Stderr (..), Stdout (..))
import ShellRun.Utils qualified as Utils
import System.Clock qualified as C
import System.Exit (ExitCode (..))
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
  IO (Either (NonNegative, Stderr) (NonNegative, Stdout))
tryTimeSh cmd path = do
  start <- C.getTime C.Monotonic
  res <- tryShExitCode cmd path
  end <- C.getTime C.Monotonic
  let diff = Utils.diffTime start end
  pure $ Bifunctor.bimap (diff,) (diff,) res
