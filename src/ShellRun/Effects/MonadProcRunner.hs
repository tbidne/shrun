-- | Provides the 'MonadProcRunner' typeclass.
--
-- @since 0.3.0.1
module ShellRun.Effects.MonadProcRunner
  ( MonadProcRunner (..),
  )
where

import ShellRun.Command (Command)
import ShellRun.IO (Stderr)
import ShellRun.Logging.RegionLogger (RegionLogger (Region))
import ShellRun.Prelude
import System.Console.Regions (ConsoleRegion)

-- | Effect for launching a process. Provides three functions of varying
-- logging behavior.
--
-- @since 0.3.0.1
class (Monad m, RegionLogger m, Region m ~ ConsoleRegion) => MonadProcRunner m where
  -- | Runs the command and returns a 'Natural' representing the duration,
  -- in terms of seconds. A failure message is returned in case of an error.
  --
  -- @since 0.3.0.1
  tryTimeProc :: Command -> m (Either (Tuple2 Natural Stderr) Natural)

  -- | We stream the commands' output like 'tryTimeProcStreamRegion' except we
  -- do __not__ create a console region. This function is intended for when we want
  -- to send command logs to a file, but do not want to stream them to the
  -- console.
  --
  -- @since 0.3.0.1
  tryTimeProcStream :: Command -> m (Either (Tuple2 Natural Stderr) Natural)

  -- | Similar to 'tryTimeProc' except we attempt to stream the commands'
  -- output to a 'Region'.
  --
  -- @since 0.3.0.1
  tryTimeProcStreamRegion :: Command -> m (Either (Tuple2 Natural Stderr) Natural)
