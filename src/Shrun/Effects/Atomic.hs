-- | Provides the 'Atomic' typeclass.
--
-- @since 0.3
module Shrun.Effects.Atomic
  ( Atomic (..),
  )
where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar qualified as TVar
import Data.IORef qualified as Ref
import Shrun.Prelude

-- | Represents a readable filesystem.
--
-- @since 0.5
class Monad m => Atomic m where
  liftSTM :: STM a -> m a

  newIORef :: a -> m (IORef a)
  readIORef :: IORef a -> m a
  modifyIORef' :: IORef a -> (a -> a) -> m ()

  newTVarIO :: a -> m (TVar a)
  readTVarIO :: TVar a -> m a

-- | @since 0.5
instance Atomic IO where
  liftSTM = atomically

  newIORef = Ref.newIORef
  readIORef = Ref.readIORef
  modifyIORef' = Ref.modifyIORef'

  newTVarIO = TVar.newTVarIO
  readTVarIO = TVar.readTVarIO

-- | @since 0.5
instance Atomic m => Atomic (ReaderT env m) where
  liftSTM = lift . liftSTM

  newIORef = lift . newIORef
  readIORef = lift . readIORef
  modifyIORef' ref = lift . modifyIORef' ref

  newTVarIO = lift . newTVarIO
  readTVarIO = lift . readTVarIO
