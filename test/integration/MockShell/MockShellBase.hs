{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides the 'MockShellBase' type.
module MockShell.MockShellBase (MockShellBase (..)) where

import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Reader qualified as MTL
import Control.Monad.Writer (MonadWriter, WriterT)
import Control.Monad.Writer qualified as MTL
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import MockEnv (MockEnv)
import ShellRun.Class.MonadLogger (MonadLogger (..))

-- | 'MockShellBase' serves as a base type for our various integration tests.
-- Its main purpose is convenience, so we do not have to re-derive various
-- typeclasses (e.g. MonadLogger).
newtype MockShellBase a = MkMockShellBase
  { runMockShellBase :: ReaderT MockEnv (WriterT [Text] Identity) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader MockEnv,
      MonadWriter
        [Text]
    )

instance MonadLogger MockShellBase where
  logNoLine = MTL.tell . pure
  logLine = MTL.tell . pure . (<> "\n")

instance Show a => Show (MockShellBase a) where
  show _ = "MkMockShellBase"
