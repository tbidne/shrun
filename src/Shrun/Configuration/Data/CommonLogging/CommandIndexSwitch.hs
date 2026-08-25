{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.CommonLogging.CommandIndexSwitch
  ( CommandIndexSwitch (..),
  )
where

import Shrun.Configuration.Data.ConfigPhase (parseSwitch)
import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude

declareFieldLabels
  [d|
    -- Type for determining if we show the command's index for display.
    newtype CommandIndexSwitch = MkCommandIndexSwitch {unCommandIndexSwitch :: Bool}
      deriving stock (Bounded, Eq, Ord, Show)
      deriving newtype (Enum)
      deriving (Pretty) via PrettySwitch
    |]

instance DecodeTOML CommandIndexSwitch where
  tomlDecoder = MkCommandIndexSwitch <$> (tomlDecoder >>= parseSwitch)

instance Default CommandIndexSwitch where
  def = MkCommandIndexSwitch False

makeFieldLabelsNoPrefix ''CommandIndexSwitch
