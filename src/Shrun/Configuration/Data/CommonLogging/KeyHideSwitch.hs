{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.CommonLogging.KeyHideSwitch
  ( KeyHideSwitch (..),
  )
where

import Shrun.Configuration.Data.ConfigPhase (parseSwitch)
import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude

declareFieldLabels
  [d|
    -- Type for determining if we use the command's key
    -- for display, rather than the key itself.
    newtype KeyHideSwitch = MkKeyHideSwitch {unKeyHideSwitch :: Bool}
      deriving stock (Bounded, Eq, Ord, Show)
      deriving newtype (Enum)
      deriving (Pretty) via PrettySwitch
    |]

instance DecodeTOML KeyHideSwitch where
  tomlDecoder = MkKeyHideSwitch <$> (tomlDecoder >>= parseSwitch)

instance Default KeyHideSwitch where
  def = MkKeyHideSwitch False

makeFieldLabelsNoPrefix ''KeyHideSwitch
