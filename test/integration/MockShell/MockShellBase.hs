-- | Provides the 'MockShellBase' type.
module MockShell.MockShellBase (MockShellBase (..)) where

import Control.Monad.Writer qualified as MTL
import Data.Functor.Identity (Identity)
import Data.String (String)
import MockEnv (MockEnv)
import ShellRun.Logging (Log (..), MonadLogger (..))
import ShellRun.Prelude

-- | 'MockShellBase' serves as a base type for our various integration tests.
-- Its main purpose is convenience, so we do not have to re-derive various
-- typeclasses (e.g. MonadLogger).
type MockShellBase :: Type -> Type
newtype MockShellBase a = MkMockShellBase
  { runMockShellBase :: ReaderT MockEnv (WriterT [Text] Identity) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader MockEnv,
      MonadWriter [Text]
    )
    via (ReaderT MockEnv (WriterT [Text] Identity))

instance MonadLogger MockShellBase where
  putLog :: Log -> MockShellBase ()
  putLog = MTL.tell . pure . msg

  clear :: MockShellBase ()
  clear = pure ()

instance Show a => Show (MockShellBase a) where
  show :: MockShellBase a -> String
  show x = "MkMockShellBase" <> show x
