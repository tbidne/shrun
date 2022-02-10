-- | Provides functionality for logging to a specific region
-- (i.e. for concurrent logging).
module ShellRun.Logging.RegionLogger
  ( -- * Typeclass for logging
    RegionLogger (..),
  )
where

import ShellRun.Logging.Log (Log (..), LogMode (..))
import ShellRun.Logging.Log qualified as Log
import ShellRun.Prelude
import System.Console.Pretty qualified as P
import System.Console.Regions (ConsoleRegion)
import System.Console.Regions qualified as Regions

-- | `RegionLogger` is a simple typeclass for abstracting logging functions.
type RegionLogger :: (Type -> Type) -> Constraint
class Monad m => RegionLogger m where
  -- | The region type for our logging. For real programs (e.g. 'IO'),
  -- this will likely be 'ConsoleRegion'.
  type Region m

  putLog :: Log -> m ()

  -- | Pushes a log to the region.
  putRegionLog :: Region m -> Log -> m ()

instance RegionLogger IO where
  type Region IO = ConsoleRegion

  putLog :: Log -> IO ()
  putLog = printLog putStrLn

  putRegionLog :: ConsoleRegion -> Log -> IO ()
  putRegionLog region lg@MkLog {mode} = do
    let logFn = case mode of
          Set -> Regions.setConsoleRegion
          Append -> Regions.appendConsoleRegion
          Finish -> Regions.finishConsoleRegion

    printLog (logFn region) lg

printLog :: (Text -> IO ()) -> Log -> IO ()
printLog fn lg@MkLog {msg} = do
  let color = Log.logToColor lg
      prefix = Log.logToPrefix lg
      log' = P.color color $ prefix <> msg
  fn log'

instance RegionLogger m => RegionLogger (ReaderT e m) where
  type Region (ReaderT e m) = Region m
  putLog = lift . putLog
  putRegionLog region = lift . putRegionLog region
