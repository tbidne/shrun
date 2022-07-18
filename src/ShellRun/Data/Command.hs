{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Command' wrapper for commands.
--
-- @since 0.1
module ShellRun.Data.Command
  ( Command (..),
  )
where

import Data.Hashable (Hashable)
import Data.String (IsString (..))
import Data.Text qualified as T
import ShellRun.Prelude

-- $setup
-- >>> :set -XOverloadedLists

-- | Wrapper for shell commands.
--
-- @since 0.1
data Command = MkCommand
  { -- | The key name for the command, for display purposes.
    --
    -- @since 0.1
    getKey :: Maybe Text,
    -- | The shell command to run.
    --
    -- @since 0.1
    command :: Text
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      Hashable
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''Command

instance IsString Command where
  fromString = MkCommand Nothing . T.pack
  {-# INLINEABLE fromString #-}
