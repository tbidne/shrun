{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.CommonLogging.KeyHideSwitch
  ( KeyHideSwitch (..),
  )
where

import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude

-- | Type for determining if we use the command's key
-- for display, rather than the key itself.
newtype KeyHideSwitch = MkKeyHideSwitch Bool
  deriving stock (Bounded, Eq, Ord, Show)
  deriving newtype (Enum)

instance DecodeTOML KeyHideSwitch where
  tomlDecoder = MkKeyHideSwitch <$> tomlDecoder

instance Default KeyHideSwitch where
  def = MkKeyHideSwitch False

instance
  (k ~ An_Iso, a ~ Bool, b ~ Bool) =>
  LabelOptic "unKeyHideSwitch" k KeyHideSwitch KeyHideSwitch a b
  where
  labelOptic = iso (\(MkKeyHideSwitch b) -> b) MkKeyHideSwitch
  {-# INLINE labelOptic #-}
