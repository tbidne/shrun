{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.FileLogging.FileLogMulti
  ( FileLogMulti (..),
    parseFileLogMulti,
    fileLogMultiMeta,
    FileLogMultiSwitch (..),
  )
where

import Shrun.Configuration.Data.ConfigPhase (parseSwitch)
import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude
import Shrun.Utils qualified as Utils

-- | File log multi config.
data FileLogMulti
  = FileLogMultiOn
  | FileLogMultiAuto
  | FileLogMultiOff
  deriving stock (Bounded, Enum, Eq, Show)

instance Default FileLogMulti where
  def = FileLogMultiOff

instance DecodeTOML FileLogMulti where
  tomlDecoder = parseFileLogMulti tomlDecoder

instance Pretty FileLogMulti where
  pretty = \case
    FileLogMultiOn -> "on"
    FileLogMultiAuto -> "auto"
    FileLogMultiOff -> "off"

parseFileLogMulti :: (MonadFail m) => m Text -> m FileLogMulti
parseFileLogMulti = (>>= Utils.inversePrettyFail "multi" fileLogMultiMeta)
{-# INLINEABLE parseFileLogMulti #-}

fileLogMultiMeta :: (IsString a) => Tuple2 Bool (List a)
fileLogMultiMeta = (False, ["on", "auto", "off"])

declareFieldLabels
  [d|
    -- Switch for logging to multiple files. Runtime companion to FileLogMulti.
    newtype FileLogMultiSwitch = MkFileLogMultiSwitch
      {unFileLogMultiSwitch :: Bool}
      deriving stock (Eq, Show)
      deriving newtype (Bounded, Enum)
      deriving (Pretty) via PrettySwitch
    |]

instance Default FileLogMultiSwitch where
  def = MkFileLogMultiSwitch False

instance DecodeTOML FileLogMultiSwitch where
  tomlDecoder = MkFileLogMultiSwitch <$> (tomlDecoder >>= parseSwitch)
