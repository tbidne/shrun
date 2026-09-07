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
data RegionLogger r :: Effect where
  LogGlobal :: forall r m. Text -> RegionLogger r m ()
  LogRegion :: forall r m. LogMode -> r -> Text -> RegionLogger r m ()
  WithRegion :: forall r m a. RegionLayout -> (r -> m a) -> RegionLogger r m a
  DisplayRegions :: forall r m a. m a -> RegionLogger r m a
  RegionList :: RegionLogger r m (TMVar [r])

type instance DispatchOf (RegionLogger _) = Dynamic

instance ShowEffect (RegionLogger r) where
  showEffectCons = \case
    LogGlobal {} -> "LogGlobal"
    LogRegion {} -> "LogRegion"
    WithRegion {} -> "WithRegion"
    DisplayRegions {} -> "DisplayRegions"
    RegionList -> "RegionList"

runRegionLogger ::
  ( r ~ ConsoleRegion,
    HasCallStack,
    IOE :> es
  ) =>
  Eff (RegionLogger r : es) a ->
  Eff es a
runRegionLogger = interpret $ \env -> \case
  LogGlobal t -> liftIO $ Prelude.putStrLn (unpack t)
  LogRegion m r t -> case m of
    LogModeSet -> liftIO $ Regions.setConsoleRegion r t
    LogModeAppend -> liftIO $ Regions.appendConsoleRegion r t
    LogModeFinish -> liftIO $ Regions.finishConsoleRegion r t
  WithRegion layout onRegion -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ Regions.withConsoleRegion layout (runInIO . onRegion)
  DisplayRegions m ->
    localSeqUnliftIO env $ \runInIO ->
      liftIO $ Regions.displayConsoleRegions (runInIO m)
  RegionList -> pure Regions.regionList

logGlobal ::
  forall r es.
  ( HasCallStack,
    RegionLogger r :> es
  ) =>
  Text ->
  Eff es ()
logGlobal = send . LogGlobal @r

logRegion ::
  forall r es.
  ( HasCallStack,
    RegionLogger r :> es
  ) =>
  LogMode ->
  r ->
  Text ->
  Eff es ()
logRegion m r = send . LogRegion @r m r

withRegion ::
  forall r es a.
  ( HasCallStack,
    RegionLogger r :> es
  ) =>
  RegionLayout ->
  (r -> Eff es a) ->
  Eff es a
withRegion l = send . WithRegion @r l

displayRegions ::
  forall r es a.
  ( HasCallStack,
    RegionLogger r :> es
  ) =>
  Eff es a ->
  Eff es a
displayRegions = send . DisplayRegions @r

regionList ::
  forall r es.
  ( HasCallStack,
    RegionLogger r :> es
  ) =>
  Eff es (TMVar [r])
regionList = send RegionList

-- | Moves the timer region to the bottom of all active regions.
--
-- See NOTE: [Restore Timer Region].
restoreTimerRegion ::
  forall r es.
  ( Concurrent :> es,
    Eq r,
    Prim :> es,
    RegionLogger r :> es
  ) =>
  IORef (Maybe r) ->
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
    moveRegionLast r = k . foldl' go ([], False)
      where
        go :: Tuple2 (List a) Bool -> a -> Tuple2 (List a) Bool
        go (acc, found) s
          | r == s = (acc, True)
          | otherwise = (s : acc, found)

        k :: Tuple2 (List a) Bool -> Tuple2 (List a) Bool
        k (acc, False) = (acc, False)
        k (acc, True) = (r : acc, True)
