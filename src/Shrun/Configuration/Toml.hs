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
        cmdLogReadSize,
        cmdLogging,
        cmdNameTrunc,
        fileLogging,
        init,
        keyHide,
        notify,
        pollInterval,
        timeout,
        timerFormat
      ),
    CoreConfigToml,
  )
import Shrun.Data.KeyHide (KeyHide)
import Shrun.Data.Legend (KeyVal)
import Shrun.Data.PollInterval (PollInterval)
import Shrun.Data.Timeout (Timeout)
import Shrun.Data.TimerFormat (TimerFormat)
import Shrun.Data.Truncation (TruncRegion (TCmdName), Truncation)
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
    keyHide <- decodeKeyHide
    pollInterval <- decodePollInterval
    cmdLogReadSize <- decodeCmdLogReadSize
    timerFormat <- decodeTimerFormat
    cmdNameTrunc <- decodecmdNameTrunc
    cmdLogging <- getFieldOptWith tomlDecoder "cmd-log"
    fileLogging <- getFieldOptWith tomlDecoder "file-log"
    notify <- getFieldOptWith tomlDecoder "notify"

    legend <- decodeLegend
    pure
      $ MkToml
        { coreConfig =
            MkCoreConfigP
              { timeout,
                init,
                keyHide,
                pollInterval,
                cmdLogReadSize,
                timerFormat,
                cmdNameTrunc,
                cmdLogging,
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
            keyHide = Nothing,
            pollInterval = Nothing,
            cmdLogReadSize = Nothing,
            timerFormat = Nothing,
            cmdNameTrunc = Nothing,
            cmdLogging = Nothing,
            fileLogging = Nothing,
            notify = Nothing
          },
      legend = Nothing
    }

decodeTimeout :: Decoder (Maybe Timeout)
decodeTimeout = getFieldOptWith tomlDecoder "timeout"

decodeInit :: Decoder (Maybe Text)
decodeInit = getFieldOptWith tomlDecoder "init"

decodeKeyHide :: Decoder (Maybe KeyHide)
decodeKeyHide = getFieldOptWith tomlDecoder "key-hide"

decodePollInterval :: Decoder (Maybe PollInterval)
decodePollInterval = getFieldOptWith tomlDecoder "poll-interval"

decodeTimerFormat :: Decoder (Maybe TimerFormat)
decodeTimerFormat = getFieldOptWith tomlDecoder "timer-format"

decodecmdNameTrunc :: Decoder (Maybe (Truncation TCmdName))
decodecmdNameTrunc = getFieldOptWith tomlDecoder "cmd-name-trunc"

decodeCmdLogReadSize :: Decoder (Maybe (Bytes B Natural))
decodeCmdLogReadSize = getFieldOptWith (fmap MkBytes tomlDecoder) "cmd-log-read-size"

decodeLegend :: Decoder (Maybe (List KeyVal))
decodeLegend = getFieldOptWith tomlDecoder "legend"
