-- | Provides the 'TimerFormat' type.
module Shrun.Data.TimerFormat
  ( -- * Type
    TimerFormat (..),

    -- * Parsing
    defaultTimerFormat,
    parseTimerFormat,
    timerFormatStr,

    -- * Formatting
    formatRelativeTime,
    formatSeconds,
  )
where

import Data.String (IsString)
import Data.Text qualified as T
import Data.Time.Relative
  ( Format (MkFormat),
    FormatStyle (FormatStyleDigital, FormatStyleProse),
    FormatVerbosity
      ( FormatVerbosityCompact,
        FormatVerbosityFull
      ),
    RelativeTime,
  )
import Data.Time.Relative qualified as RT
import Shrun.Prelude

-- | Determines how to format the timer.
data TimerFormat
  = DigitalCompact
  | DigitalFull
  | ProseCompact
  | ProseFull
  deriving stock (Eq, Show)

instance DecodeTOML TimerFormat where
  tomlDecoder = tomlDecoder >>= parseTimerFormat

-- | Prose Compact
defaultTimerFormat :: TimerFormat
defaultTimerFormat = ProseCompact

-- | Parse timer format.
parseTimerFormat :: (MonadFail m) => Text -> m TimerFormat
parseTimerFormat "digital_compact" = pure DigitalCompact
parseTimerFormat "digital_full" = pure DigitalFull
parseTimerFormat "prose_compact" = pure ProseCompact
parseTimerFormat "prose_full" = pure ProseFull
parseTimerFormat bad = fail $ "Unrecognized timer-format: " <> unpack bad

-- | Available 'TimerFormat' strings.
timerFormatStr :: (IsString a) => a
timerFormatStr = "(digital_compact|digital_full|prose_compact|prose_full)"

-- | Formats a relative time.
formatRelativeTime :: TimerFormat -> RelativeTime -> Text
formatRelativeTime fmt =
  T.pack . RT.formatRelativeTime (toRelativeTimeFormat fmt)

-- | Formats a relative time seconds.
formatSeconds :: TimerFormat -> Natural -> Text
formatSeconds fmt =
  T.pack . RT.formatSeconds (toRelativeTimeFormat fmt)

toRelativeTimeFormat :: TimerFormat -> Format
toRelativeTimeFormat DigitalCompact = MkFormat FormatStyleDigital FormatVerbosityCompact
toRelativeTimeFormat DigitalFull = MkFormat FormatStyleDigital FormatVerbosityFull
toRelativeTimeFormat ProseCompact = MkFormat FormatStyleProse FormatVerbosityCompact
toRelativeTimeFormat ProseFull = MkFormat FormatStyleProse FormatVerbosityFull
