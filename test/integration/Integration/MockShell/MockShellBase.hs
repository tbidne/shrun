{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'MockShellBase' type.
module Integration.MockShell.MockShellBase
  ( MockShellBase,
    runMockShellBase,
  )
where

import Integration.MockEnv (MockEnv)
import Integration.Prelude
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import System.Console.Regions (ConsoleRegion)

-- | 'MockShellBase' serves as a base type for our various integration tests.
-- Its main purpose is convenience, so we do not have to re-derive various
-- typeclasses (e.g. MonadLogger).
type MockShellBase :: Type -> Type
newtype MockShellBase a = MkMockShellBase (ReaderT MockEnv IO a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadCatch,
      MonadIO,
      MonadMask,
      MonadReader MockEnv,
      MonadThrow,
      MonadUnliftIO
    )
    via (ReaderT MockEnv IO)

runMockShellBase :: MockShellBase a -> MockEnv -> IO a
runMockShellBase (MkMockShellBase rdr) = runReaderT rdr

instance RegionLogger MockShellBase where
  type Region MockShellBase = ConsoleRegion
  logFn logTxt = do
    ls <- asks $ view #logs
    liftIO $ atomically $ modifyTVar' ls (logTxt :)
  logModeToRegionFn _ _ = logFn
