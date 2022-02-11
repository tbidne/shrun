-- | Provides the 'MockShellBase' type.
module MockShell.MockShellBase (MockShellBase (..)) where

import Data.Functor.Identity (Identity)
import Data.String (String)
import MockEnv (MockEnv)
import ShellRun.Logging.Log (Log (..))
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import ShellRun.Prelude

-- | 'MockShellBase' serves as a base type for our various integration tests.
-- Its main purpose is convenience, so we do not have to re-derive various
-- typeclasses (e.g. MonadLogger).
type MockShellBase :: Type -> Type
newtype MockShellBase a = MkMockShellBase
  { runMockShellBase :: ReaderT MockEnv (WriterT (List Text) Identity) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader MockEnv,
      MonadWriter (List Text)
    )
    via (ReaderT MockEnv (WriterT (List Text) Identity))

instance RegionLogger MockShellBase where
  type Region MockShellBase = ()
  putLog :: Log -> MockShellBase ()
  putLog = tell . pure . msg

  putRegionLog _ = putLog

instance Show a => Show (MockShellBase a) where
  show :: MockShellBase a -> String
  show x = "MkMockShellBase" <> show x
