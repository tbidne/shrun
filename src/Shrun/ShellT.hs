{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'ShellT' monad transformer.
--
-- @since 0.1
module Shrun.ShellT
  ( ShellT,
    runShellT,
  )
where

import Shrun.Configuration.Env.Types (Env)
import Shrun.Logging (MonadRegionLogger (..))
import Shrun.Prelude

-- | `ShellT` is the main application type that runs shell commands.
--
-- @since 0.1
type ShellT :: Type -> (Type -> Type) -> Type -> Type
newtype ShellT env m a = MkShellT (ReaderT env m a)
  deriving
    ( -- | @since 0.1
      Functor,
      -- | @since 0.1
      Applicative,
      -- | @since 0.1
      Monad,
      -- | @since X-X-X
      MonadAsync,
      -- | @since 0.1
      MonadReader env,
      -- | @since 0.1
      MonadCatch,
      -- | @since 0.6
      MonadCallStack,
      -- | @since X-X-X
      MonadFileReader,
      -- | @since X-X-X
      MonadFileWriter,
      -- | @since X-X-X
      MonadHandleReader,
      -- | @since X-X-X
      MonadHandleWriter,
      -- | @since 0.1
      MonadIO,
      -- | @since 0.6
      MonadIORef,
      -- | @since 0.1
      MonadMask,
      -- @since X-X-X
      MonadPathWriter,
      -- @since X-X-X
      MonadProcess,
      -- | @since X-X-X
      MonadSTM,
      -- | @since 0.6
      MonadTerminal,
      -- | @since 0.6
      MonadThread,
      -- | @since 0.5
      MonadTime,
      -- | @since 0.1
      MonadThrow
    )
    via (ReaderT env m)

-- | Runs a 'ShellT' with the given @env@.
--
-- @since 0.1
runShellT :: ShellT env m a -> env -> m a
runShellT (MkShellT rdr) = runReaderT rdr
{-# INLINEABLE runShellT #-}

-- Concrete Env here so we can vary our logging logic with other envs
-- (i.e. in tests).

-- Can't use @deriving via m@ due to a bug: @HC version 9.2.5: No skolem info:@.
-- https://gitlab.haskell.org/ghc/ghc/-/issues/15376

-- | @since 0.1
deriving newtype instance MonadRegionLogger m => MonadRegionLogger (ShellT Env m)
