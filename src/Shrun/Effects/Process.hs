-- | Provides the 'Process' typeclass.
--
-- @since 0.5
module Shrun.Effects.Process
  ( Process (..),
    tryTimeCmd,
  )
where

import Data.Time.Relative (RelativeTime)
import Shrun.Data.Command (Command)
import Shrun.Effects.Timing
import Shrun.IO (Stderr)
import Shrun.Logging.RegionLogger (RegionLogger (Region))
import Shrun.Prelude
import System.Console.Regions (ConsoleRegion)

-- | Effect for launching a process. Provides three functions of varying
-- logging behavior.
--
-- @since 0.5
class (Monad m, RegionLogger m, Region m ~ ConsoleRegion) => Process m where
  -- | Runs the command and returns an error message on failure.
  --
  -- @since 0.5
  tryCmd :: Command -> m (Maybe Stderr)

  -- | We stream the commands' output like 'tryStreamRegion' except we
  -- do __not__ create a console region. This function is intended for when we
  -- want to send command logs to a file, but do not want to stream them to the
  -- console.
  --
  -- @since 0.5
  tryCmdStream :: Command -> m (Maybe Stderr)

  -- | Similar to 'tryCmd' except we attempt to stream the commands'
  -- output to a 'Region'.
  --
  -- @since 0.5
  tryCmdStreamRegion :: Command -> m (Maybe Stderr)

-- | Runs a command with timing. Returns an 'Either' so that the user is
-- forced to check for failures.
--
-- @since 0.5
tryTimeCmd :: (Process m, Timing m) => (Command -> m (Maybe Stderr)) -> Command -> m (Either (Tuple2 RelativeTime Stderr) RelativeTime)
tryTimeCmd runner cmd =
  withTiming (runner cmd) >>= \case
    (rt, Nothing) -> pure $ Right rt
    (rt, Just err) -> pure $ Left (rt, err)
