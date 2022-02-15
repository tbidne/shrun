-- | Provides types for the legend functionality.
--
-- @since 0.1.0.0
module ShellRun.Data.Legend
  ( LegendErr (..),
    LegendMap,
  )
where

import Data.HashMap.Strict (HashMap)
import ShellRun.Prelude

-- | Various errors that can occur while processing the legend.
--
-- @since 0.1.0.0
data LegendErr
  = -- | Errors relating to locating the legend file itself.
    --
    -- @since 0.1.0.0
    FileErr Text
  | -- | Errors relating to legend key=val format.
    --
    -- @since 0.1.0.0
    EntryErr Text
  | -- | Errors relating to cyclic keys.
    --
    -- @since 0.1.0.0
    CyclicKeyErr Text
  | -- | Errors relating to duplicate keys.
    --
    -- @since 0.1.0.0
    DuplicateKeyErr Text
  deriving (Eq, Show)

-- | Alias for our legend map.
--
-- @since 0.1.0.0
type LegendMap = HashMap Text Text
