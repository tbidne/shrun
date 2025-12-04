module Shrun.Configuration.Data.StripControl
  ( StripControl (..),
    parseStripControl,
    ConsoleLogStripControl,
    FileLogStripControl,
    stripControlStr,
  )
where

import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude
import Shrun.Utils qualified as Utils

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

instance Pretty (StripControl t) where
  pretty = \case
    StripControlSmart -> "smart"
    StripControlNone -> "off"
    StripControlAll -> "all"

type ConsoleLogStripControl = StripControl StripControlConsoleLog

type FileLogStripControl = StripControl StripControlFileLog

parseStripControl :: (MonadFail m) => m Text -> m (StripControl t)
parseStripControl getTxt =
  getTxt >>= \case
    "all" -> pure StripControlAll
    -- "off" over "none" just for consistency with other options.
    "off" -> pure StripControlNone
    "smart" -> pure StripControlSmart
    bad ->
      fail
        $ Utils.fmtUnrecognizedError
          "strip control"
          stripControlStr
          (unpack bad)

instance Default ConsoleLogStripControl where
  def = StripControlSmart

instance Default FileLogStripControl where
  def = StripControlAll

stripControlStr :: String
stripControlStr = "(all | smart | off)"
