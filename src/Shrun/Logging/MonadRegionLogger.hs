-- | Provides functionality for logging to a specific region
-- (i.e. for concurrent console logging).
module Shrun.Logging.MonadRegionLogger
  ( MonadRegionLogger (..),
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

-- | `MonadRegionLogger` is a simple typeclass for abstracting logging functions.
type MonadRegionLogger :: (Type -> Type) -> Constraint
class (Eq (Region m), Monad m) => MonadRegionLogger m where
  -- | The type of the region. This will be ConsoleRegion for production
  -- code and () for tests.
  type Region m

  -- | Pushes a log to the "global" region.
  logGlobal :: (HasCallStack) => Text -> m ()

  -- | Pushes a log to the region.
  logRegion :: (HasCallStack) => LogMode -> Region m -> Text -> m ()

  -- | Runs an @m a@ with a region.
  withRegion :: (HasCallStack) => RegionLayout -> (Region m -> m a) -> m a

  -- | Displays the regions. This should wrap whatever top-level logic
  -- needs regions.
  displayRegions :: (HasCallStack) => m a -> m a

  -- | Retrieve all regions.
  regionList :: m (TMVar [Region m])

instance MonadRegionLogger IO where
  type Region IO = ConsoleRegion

  logGlobal = putTextLn

  logRegion LogModeSet cr = Regions.setConsoleRegion cr
  logRegion LogModeAppend cr = Regions.appendConsoleRegion cr
  logRegion LogModeFinish cr = Regions.finishConsoleRegion cr

  withRegion = Regions.withConsoleRegion

  displayRegions = Regions.displayConsoleRegions

  regionList = pure Regions.regionList

instance (MonadRegionLogger m) => MonadRegionLogger (ReaderT env m) where
  type Region (ReaderT env m) = Region m

  logGlobal = lift . logGlobal

  logRegion m r = lift . logRegion m r

  withRegion l f =
    ask >>= \e -> lift (withRegion l (\r -> runReaderT (f r) e))

  displayRegions m = ask >>= \e -> lift (displayRegions $ runReaderT m e)

  regionList = lift regionList

-- | Moves the timer region to the bottom of all active regions.
--
-- See NOTE: [Restore Timer Region].
restoreTimerRegion ::
  forall m.
  ( MonadIORef m,
    MonadRegionLogger m,
    MonadSTM m
  ) =>
  IORef (Maybe (Region m)) ->
  m ()
restoreTimerRegion timerRegionRef = do
  mRegion <- readIORef' timerRegionRef
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
