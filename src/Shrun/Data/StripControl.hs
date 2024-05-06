module Shrun.Data.StripControl
  ( StripControl (..),
    parseStripControl,
    ConsoleLogStripControl,
    FileLogStripControl,
  )
where

import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude

data StripControlType
  = StripControlConsoleLog
  | StripControlFileLog

-- | Determines how we should treat control characters encountered in
-- logs.
type StripControl :: StripControlType -> Type
data StripControl t
  = -- | \"Intelligently\" strip control characters e.g. colors are fine,
    -- ones that affect the cursor should be removed.
    StripControlSmart
  | -- | Do not strip any control characters.
    StripControlNone
  | -- | Strip all control characters.
    StripControlAll
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance DecodeTOML (StripControl t) where
  tomlDecoder = parseStripControl tomlDecoder

type ConsoleLogStripControl = StripControl StripControlConsoleLog

type FileLogStripControl = StripControl StripControlFileLog

parseStripControl :: (MonadFail m) => m Text -> m (StripControl t)
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

instance Default ConsoleLogStripControl where
  def = StripControlSmart

instance Default FileLogStripControl where
  def = StripControlAll
