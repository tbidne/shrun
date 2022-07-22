-- | Provides the 'Mutable' typeclass.
--
-- @since 0.3
module Shrun.Effects.Mutable
  ( Mutable (..),
  )
where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar qualified as TVar
import Data.IORef qualified as Ref
import Shrun.Prelude

-- | Represents mutable operations e.g. STM, IORefs.
--
-- @since 0.5
class Monad m => Mutable m where
  -- | @since 0.5
  liftSTM :: STM a -> m a

  -- | @since 0.5
  newIORef :: a -> m (IORef a)

  -- | @since 0.5
  readIORef :: IORef a -> m a

  -- | @since 0.5
  modifyIORef' :: IORef a -> (a -> a) -> m ()

  -- | @since 0.5
  newTVarIO :: a -> m (TVar a)

  -- | @since 0.5
  readTVarIO :: TVar a -> m a

-- | @since 0.5
instance Mutable IO where
  liftSTM = atomically

  newIORef = Ref.newIORef
  readIORef = Ref.readIORef
  modifyIORef' = Ref.modifyIORef'

  newTVarIO = TVar.newTVarIO
  readTVarIO = TVar.readTVarIO

-- | @since 0.5
instance Mutable m => Mutable (ReaderT env m) where
  liftSTM = lift . liftSTM

  newIORef = lift . newIORef
  readIORef = lift . readIORef
  modifyIORef' ref = lift . modifyIORef' ref

  newTVarIO = lift . newTVarIO
  readTVarIO = lift . readTVarIO
