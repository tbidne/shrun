module Shrun.Data.KeyHide
  ( KeyHide (..),
  )
where

import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude

-- | Type for determining if we use the command's key
-- for display, rather than the key itself.
data KeyHide
  = -- | Display the command's key, if it exists, rather
    -- than the key itself.
    KeyHideOff
  | -- | Display the command itself, not the key.
    KeyHideOn
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance DecodeTOML KeyHide where
  tomlDecoder =
    tomlDecoder <&> \case
      True -> KeyHideOn
      False -> KeyHideOff

instance Default KeyHide where
  def = KeyHideOff
