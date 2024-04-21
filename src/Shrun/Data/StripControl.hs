module Shrun.Data.StripControl
  ( StripControl (..),
    parseStripControl,
    defaultCmdLogStripControl,
    defaultFileLogStripControl,
  )
where

import Shrun.Prelude

-- | Determines how we should treat control characters encountered in
-- logs.
data StripControl
  = -- | \"Intelligently\" strip control characters e.g. colors are fine,
    -- ones that affect the cursor should be removed.
    StripControlSmart
  | -- | Do not strip any control characters.
    StripControlNone
  | -- | Strip all control characters.
    StripControlAll
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance DecodeTOML StripControl where
  tomlDecoder = parseStripControl tomlDecoder

parseStripControl :: (MonadFail m) => m Text -> m StripControl
parseStripControl getTxt =
  getTxt >>= \case
    "all" -> pure StripControlAll
    "none" -> pure StripControlNone
    "smart" -> pure StripControlSmart
    bad ->
      fail
        $ mconcat
          [ "Wanted one of (all|none|smart), received: ",
            unpack bad
          ]

defaultCmdLogStripControl :: StripControl
defaultCmdLogStripControl = StripControlSmart

defaultFileLogStripControl :: StripControl
defaultFileLogStripControl = StripControlAll
