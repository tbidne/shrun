{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Toml
  ( Toml (..),
  )
where

import Shrun.Configuration.Data.Core
  ( CoreConfigP
      ( MkCoreConfigP,
        commandLogging,
        commonLogging,
        consoleLogging,
        fileLogging,
        init,
        notify,
        timeout
      ),
    CoreConfigToml,
  )
import Shrun.Configuration.Data.Core.Timeout (Timeout)
import Shrun.Configuration.Default (Default (def))
import Shrun.Configuration.Toml.Legend (KeyVal)
import Shrun.Prelude

-- | Holds toml config.
data Toml = MkToml
  { -- | Core config.
    coreConfig :: CoreConfigToml,
    -- | Legend.
    legend :: Maybe (List KeyVal)
  }
  deriving stock (Eq, Show)

instance
  (k ~ A_Lens, a ~ CoreConfigToml, b ~ CoreConfigToml) =>
  LabelOptic "coreConfig" k Toml Toml a b
  where
  labelOptic = lensVL $ \f (MkToml _coreConfig _legend) ->
    fmap (`MkToml` _legend) (f _coreConfig)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe (List KeyVal), b ~ Maybe (List KeyVal)) =>
  LabelOptic "legend" k Toml Toml a b
  where
  labelOptic = lensVL $ \f (MkToml _coreConfig _legend) ->
    fmap (MkToml _coreConfig) (f _legend)
  {-# INLINE labelOptic #-}

instance Default Toml where
  def =
    MkToml
      { coreConfig = def,
        legend = def
      }

instance DecodeTOML Toml where
  tomlDecoder = do
    timeout <- decodeTimeout
    init <- decodeInit
    commonLogging <- getFieldOptWith tomlDecoder "common-log"
    commandLogging <- getFieldOptWith tomlDecoder "command-log"
    consoleLogging <- getFieldOptWith tomlDecoder "console-log"
    fileLogging <- getFieldOptWith tomlDecoder "file-log"
    notify <- getFieldOptWith tomlDecoder "notify"

    legend <- decodeLegend
    pure
      $ MkToml
        { coreConfig =
            MkCoreConfigP
              { timeout,
                init,
                commonLogging,
                commandLogging,
                consoleLogging,
                fileLogging,
                notify
              },
          legend
        }

decodeTimeout :: Decoder (Maybe Timeout)
decodeTimeout = getFieldOptWith tomlDecoder "timeout"

decodeInit :: Decoder (Maybe Text)
decodeInit = getFieldOptWith tomlDecoder "init"

decodeLegend :: Decoder (Maybe (List KeyVal))
decodeLegend = getFieldOptWith tomlDecoder "legend"
