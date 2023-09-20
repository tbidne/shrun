{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides functionality for logging to a specific region
-- (i.e. for concurrent console logging).
module Shrun.Logging.RegionLogger
  ( -- * Effect
    RegionLoggerDynamic (..),
    logGlobal,
    logRegion,
    withRegion,
    displayRegions,

    -- ** Handlers
    runRegionLoggerDynamicIO,
  )
where

import Effectful (Dispatch (Dynamic), DispatchOf, Effect)
import Effectful.Dispatch.Dynamic (localSeqUnliftIO, reinterpret, send)
import Effectful.Terminal.Dynamic (runTerminalDynamicIO)
import Shrun.Logging.Types.Internal
  ( LogMode
      ( LogModeAppend,
        LogModeFinish,
        LogModeSet
      ),
  )
import Shrun.Prelude
import System.Console.Regions qualified as Regions

-- | Dynamic effect for region logging.
data RegionLoggerDynamic :: Type -> Effect where
  LogGlobal :: Text -> RegionLoggerDynamic r es ()
  LogRegion :: LogMode -> r -> Text -> RegionLoggerDynamic r m ()
  WithRegion :: RegionLayout -> (r -> m a) -> RegionLoggerDynamic r m a
  DisplayRegions :: m a -> RegionLoggerDynamic r m a

type instance DispatchOf (RegionLoggerDynamic _) = Dynamic

logGlobal :: forall r es. (RegionLoggerDynamic r :> es) => Text -> Eff es ()
logGlobal = send @(RegionLoggerDynamic r) . LogGlobal

logRegion :: (RegionLoggerDynamic r :> es) => LogMode -> r -> Text -> Eff es ()
logRegion m r = send . LogRegion m r

withRegion :: (RegionLoggerDynamic r :> es) => RegionLayout -> (r -> Eff es a) -> Eff es a
withRegion l = send . WithRegion l

displayRegions :: forall r es a. (RegionLoggerDynamic r :> es) => Eff es a -> Eff es a
displayRegions = send @(RegionLoggerDynamic r) . DisplayRegions

runRegionLoggerDynamicIO ::
  ( IOE :> es
  ) =>
  Eff (RegionLoggerDynamic ConsoleRegion : es) a ->
  Eff es a
runRegionLoggerDynamicIO = reinterpret runTerminalDynamicIO $ \env -> \case
  LogGlobal t -> putTextLn t
  LogRegion LogModeSet cr t -> liftIO $ Regions.setConsoleRegion cr t
  LogRegion LogModeAppend cr t -> liftIO $ Regions.appendConsoleRegion cr t
  LogRegion LogModeFinish cr t -> liftIO $ Regions.finishConsoleRegion cr t
  WithRegion l onRegion -> localSeqUnliftIO env $ \runInIO ->
    Regions.withConsoleRegion l (runInIO . onRegion)
  DisplayRegions m -> localSeqUnliftIO env $ \runInIO ->
    Regions.displayConsoleRegions (runInIO m)
