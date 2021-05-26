{-# LANGUAGE ImportQualifiedPost #-}

module MockShell.MockShellBase (MockShellBase (..)) where

import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Reader qualified as MTL
import Control.Monad.Writer (MonadWriter, WriterT)
import Control.Monad.Writer qualified as MTL
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import ShellRun.Class.MonadLogger (MonadLogger (..))
import ShellRun.Types.Env (Env)

newtype MockShellBase a = MkMockShellBase
  { runMockShellBase :: ReaderT Env (WriterT [Text] Identity) a
  }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadWriter [Text])

instance MonadLogger MockShellBase where
  logNoLine = MTL.tell . pure
  logLine = MTL.tell . pure . (<> "\n")

instance Show a => Show (MockShellBase a) where
  show _ = "MkMockShellBase"