{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Toml
  ( Toml (..),
    defaultToml,
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

makeFieldLabelsNoPrefix ''Toml

instance DecodeTOML Toml where
  tomlDecoder = do
    timeout <- decodeTimeout
    init <- decodeInit
    commonLogging <- getFieldOptWith tomlDecoder "log"
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

defaultToml :: Toml
defaultToml =
  MkToml
    { coreConfig =
        MkCoreConfigP
          { timeout = Nothing,
            init = Nothing,
            commonLogging = Nothing,
            consoleLogging = Nothing,
            commandLogging = Nothing,
            fileLogging = Nothing,
            notify = Nothing
          },
      legend = Nothing
    }

decodeTimeout :: Decoder (Maybe Timeout)
decodeTimeout = getFieldOptWith tomlDecoder "timeout"

decodeInit :: Decoder (Maybe Text)
decodeInit = getFieldOptWith tomlDecoder "init"

decodeLegend :: Decoder (Maybe (List KeyVal))
decodeLegend = getFieldOptWith tomlDecoder "legend"
