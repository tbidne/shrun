-- | Provides the 'TimedProcess' typeclass.
--
-- @since 0.3.0.1
module Shrun.Effects.TimedProcess
  ( TimedProcess (..),
  )
where

import Data.Time.Relative (RelativeTime)
import Shrun.Data.Command (Command)
import Shrun.IO (Stderr)
import Shrun.Logging.RegionLogger (RegionLogger (Region))
import Shrun.Prelude
import System.Console.Regions (ConsoleRegion)

-- | Effect for launching a process. Provides three functions of varying
-- logging behavior.
--
-- @since 0.3.0.1
class (Monad m, RegionLogger m, Region m ~ ConsoleRegion) => TimedProcess m where
  -- | Runs the command and returns a 'RelativeTime' representing the duration.
  -- A failure message is returned in case of an error.
  --
  -- @since 0.3.0.1
  tryTime :: Command -> m (Either (Tuple2 RelativeTime Stderr) RelativeTime)

  -- | We stream the commands' output like 'tryTimeStreamRegion' except we
  -- do __not__ create a console region. This function is intended for when we
  -- want to send command logs to a file, but do not want to stream them to the
  -- console.
  --
  -- @since 0.3.0.1
  tryTimeStream :: Command -> m (Either (Tuple2 RelativeTime Stderr) RelativeTime)

  -- | Similar to 'tryTime' except we attempt to stream the commands'
  -- output to a 'Region'.
  --
  -- @since 0.3.0.1
  tryTimeStreamRegion :: Command -> m (Either (Tuple2 RelativeTime Stderr) RelativeTime)
