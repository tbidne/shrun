-- | Provides the 'Terminal' typeclass.
--
-- @since 0.3
module Shrun.Effects.Terminal
  ( Terminal (..),
  )
where

import Shrun.Prelude
import System.Console.Terminal.Size (Window (..), size)

-- | @since 0.5
data TermSizeException = MkTermSizeException
  deriving stock
    ( -- | @since 0.5
      Eq,
      -- | @since 0.5
      Show
    )

-- | @since 0.5
instance Exception TermSizeException where
  displayException = const "Failed to detect the terminal size."

-- | Represents a terminal.
--
-- @since 0.5
class Monad m => Terminal m where
  -- | Simple print function.
  --
  -- @since 0.5
  putTextLn :: Text -> m ()

  -- | Retrieves the terminal width.
  --
  -- @since 0.5
  getTerminalWidth :: m Natural

  -- | Runs sleep in the current thread.
  --
  -- @since 0.5
  sleep :: Int -> m ()

-- | @since 0.5
instance Terminal IO where
  putTextLn = putStrLn
  getTerminalWidth =
    (width <<$>> size) >>= \case
      Just h -> pure h
      Nothing -> throwIO MkTermSizeException
  sleep = threadDelay

-- | @since 0.5
instance Terminal m => Terminal (ReaderT env m) where
  putTextLn = lift . putTextLn
  getTerminalWidth = lift getTerminalWidth
  sleep = lift . sleep
