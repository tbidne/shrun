{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal module for logging types.
--
-- @since 0.1
module Shrun.Logging.Types.Internal
  ( FileLog (..),
    ConsoleLog (..),
  )
where

import Shrun.Prelude

-- | 'FileLog' is a textual representation of a given 'Log'. No coloring
-- is included, but we include the prefix (e.g. Warn) along with a timestamp.
--
-- @since 0.7
newtype FileLog = UnsafeFileLog
  { -- | @since 0.7
    unFileLog :: Text
  }
  deriving stock
    ( -- | @since 0.7
      Eq,
      -- | @since 0.7
      Show
    )

-- | @since 0.7
makeFieldLabelsWith
  (noPrefixFieldLabels & generateUpdateableOptics .~ False)
  ''FileLog

-- | 'ConsoleLog' is a textual representation of a given 'Log'.
--
-- @since 0.7
newtype ConsoleLog = UnsafeConsoleLog
  { -- | @since 0.7
    unConsoleLog :: Text
  }
  deriving stock
    ( -- | @since 0.7
      Eq,
      -- | @since 0.7
      Show
    )

-- | @since 0.7
makeFieldLabelsWith
  (noPrefixFieldLabels & generateUpdateableOptics .~ False)
  ''ConsoleLog
