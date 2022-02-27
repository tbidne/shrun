{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'MockShellBase' type.
module Integration.MockShell.MockShellBase
  ( MockShellBase,
    runMockShellBase,
  )
where

import Data.Functor.Identity (Identity (..))
import Integration.MockEnv (MockEnv)
import Integration.Prelude
import ShellRun.Logging.RegionLogger (RegionLogger (..))

-- | 'MockShellBase' serves as a base type for our various integration tests.
-- Its main purpose is convenience, so we do not have to re-derive various
-- typeclasses (e.g. MonadLogger).
type MockShellBase :: Type -> Type
newtype MockShellBase a = MkMockShellBase (ReaderT MockEnv (WriterT (List Text) Identity) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader MockEnv,
      MonadWriter (List Text)
    )
    via (ReaderT MockEnv (WriterT (List Text) Identity))

runMockShellBase :: MockShellBase a -> MockEnv -> (a, List Text)
runMockShellBase (MkMockShellBase rdr) = runIdentity . runWriterT . runReaderT rdr

instance RegionLogger MockShellBase where
  type Region MockShellBase = ()
  putLog = tell . pure . view #msg
  putRegionLog _ = putLog

instance Show a => Show (MockShellBase a) where
  show x = "MkMockShellBase" <> show x
