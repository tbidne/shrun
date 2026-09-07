{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides functionality for logging to a specific region
-- (i.e. for concurrent console logging).
module Shrun.Logging.RegionLogger
  ( -- * Effect
    RegionLogger (..),
    logGlobal,
    logRegion,
    withRegion,
    displayRegions,
    regionList,

    -- * Handler
    runRegionLogger,

    -- * Functions
    restoreTimerRegion,
  )
where

import Control.Concurrent.STM.TMVar qualified as TMVar
import Shrun.Logging.Types.Internal
  ( LogMode
      ( LogModeAppend,
        LogModeFinish,
        LogModeSet
      ),
  )
import Shrun.Prelude
import System.Console.Regions qualified as Regions
import Prelude qualified

type RegionLogger :: Type -> Effect
data RegionLogger rgn :: Effect where
  LogGlobal :: forall rgn m. Text -> RegionLogger rgn m ()
  LogRegion :: forall rgn m. LogMode -> rgn -> Text -> RegionLogger rgn m ()
  WithRegion :: forall rgn m a. RegionLayout -> (rgn -> m a) -> RegionLogger rgn m a
  DisplayRegions :: forall rgn m a. m a -> RegionLogger rgn m a
  RegionList :: RegionLogger rgn m (TMVar [rgn])

type instance DispatchOf (RegionLogger _) = Dynamic

instance ShowEffect (RegionLogger rgn) where
  showEffectCons = \case
    LogGlobal {} -> "LogGlobal"
    LogRegion {} -> "LogRegion"
    WithRegion {} -> "WithRegion"
    DisplayRegions {} -> "DisplayRegions"
    RegionList -> "RegionList"

runRegionLogger ::
  ( rgn ~ ConsoleRegion,
    HasCallStack,
    IOE :> es
  ) =>
  Eff (RegionLogger rgn : es) a ->
  Eff es a
runRegionLogger = interpret $ \env -> \case
  LogGlobal t -> liftIO $ Prelude.putStrLn (unpack t)
  LogRegion m rgn t -> case m of
    LogModeSet -> liftIO $ Regions.setConsoleRegion rgn t
    LogModeAppend -> liftIO $ Regions.appendConsoleRegion rgn t
    LogModeFinish -> liftIO $ Regions.finishConsoleRegion rgn t
  WithRegion layout onRegion -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ Regions.withConsoleRegion layout (runInIO . onRegion)
  DisplayRegions m ->
    localSeqUnliftIO env $ \runInIO ->
      liftIO $ Regions.displayConsoleRegions (runInIO m)
  RegionList -> pure Regions.regionList

logGlobal ::
  forall rgn es.
  ( HasCallStack,
    RegionLogger rgn :> es
  ) =>
  Text ->
  Eff es ()
logGlobal = send . LogGlobal @rgn

logRegion ::
  forall rgn es.
  ( HasCallStack,
    RegionLogger rgn :> es
  ) =>
  LogMode ->
  rgn ->
  Text ->
  Eff es ()
logRegion m rgn = send . LogRegion @rgn m rgn

withRegion ::
  forall rgn es a.
  ( HasCallStack,
    RegionLogger rgn :> es
  ) =>
  RegionLayout ->
  (rgn -> Eff es a) ->
  Eff es a
withRegion l = send . WithRegion @rgn l

displayRegions ::
  forall rgn es a.
  ( HasCallStack,
    RegionLogger rgn :> es
  ) =>
  Eff es a ->
  Eff es a
displayRegions = send . DisplayRegions @rgn

regionList ::
  forall rgn es.
  ( HasCallStack,
    RegionLogger rgn :> es
  ) =>
  Eff es (TMVar [rgn])
regionList = send RegionList

-- | Moves the timer region to the bottom of all active regions.
--
-- See NOTE: [Restore Timer Region].
restoreTimerRegion ::
  forall rgn es.
  ( Concurrent :> es,
    Eq rgn,
    Prim :> es,
    RegionLogger rgn :> es
  ) =>
  IORef (Maybe rgn) ->
  Eff es ()
restoreTimerRegion timerRegionRef = do
  mRegion <- readIORef timerRegionRef
  case mRegion of
    Nothing -> pure ()
    Just region -> do
      regionsVar <- regionList
      atomically $ TMVar.tryReadTMVar regionsVar >>= \case
        Nothing -> pure ()
        Just allRegions -> do
          let (allRegions', changed) = moveRegionLast region allRegions
          -- Doesn't seem necessary, but maybe worth only messing with the UI
          -- when there has strictly been a change. At the very least we may
          -- want this if we move these function calls to the timer
          -- (i.e. every second).
          when changed $ TMVar.writeTMVar regionsVar allRegions'
  where
    -- Searches for the given element in the list. Removes all occurences,
    -- and if any were found, prepends it to the new list.
    -- I _thought_ I had to end this with a 'reverse' as foldl' reverses the
    -- list. But for whatever reason, the appears not to be the case.
    --
    -- More precisely, without the reverse, the TimerRegion (what we are
    -- searching for) ends up on the bottom, as we want. With the reverse,
    -- it is succeeded by the command logs. I am not sure why.
    moveRegionLast :: forall a. (Eq a) => a -> List a -> Tuple2 (List a) Bool
    moveRegionLast rgn = k . foldl' go ([], False)
      where
        go :: Tuple2 (List a) Bool -> a -> Tuple2 (List a) Bool
        go (acc, found) s
          | rgn == s = (acc, True)
          | otherwise = (s : acc, found)

        k :: Tuple2 (List a) Bool -> Tuple2 (List a) Bool
        k (acc, False) = (acc, False)
        k (acc, True) = (rgn : acc, True)
