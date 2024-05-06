module Shrun.Configuration.Data.CommonLogging.KeyHideSwitch
  ( KeyHideSwitch (..),
  )
where

import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude

-- | Type for determining if we use the command's key
-- for display, rather than the key itself.
data KeyHideSwitch
  = -- | Display the command's key, if it exists, rather
    -- than the key itself.
    KeyHideOff
  | -- | Display the command itself, not the key.
    KeyHideOn
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance DecodeTOML KeyHideSwitch where
  tomlDecoder =
    tomlDecoder <&> \case
      True -> KeyHideOn
      False -> KeyHideOff

instance Default KeyHideSwitch where
  def = KeyHideOff
