{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'TomlConfig' type.
--
-- @since 0.5
module Shrun.Configuration.Toml
  ( TomlConfig (..),
    argsToTomlConfig,
  )
where

import Shrun.Configuration.Args (Args (..), FileMode (..), FileSizeMode (..))
import Shrun.Configuration.Env.Types
  ( CmdDisplay,
    CmdLogging,
    LineTruncation,
    StripControl,
    TruncRegion (..),
    Truncation,
  )
import Shrun.Data.FilePathDefault (FilePathDefault)
import Shrun.Data.Legend (KeyVal)
import Shrun.Data.Timeout (Timeout)
import Shrun.Prelude

-- | 'TomlConfig' refers to TomlConfiguration we retrieve from the toml TomlConfig file.
--
-- @since 0.5
data TomlConfig = MkTomlConfig
  { -- | Timeout.
    --
    -- @since 0.5
    timeout :: !(Maybe Timeout),
    -- | Optional file logging. If enabled, holds the path to the file
    -- and the log queue.
    --
    -- @since 0.5
    fileLogging :: !(Maybe FilePathDefault),
    -- | Mode to use with the file log.
    --
    -- since 0.5
    fileLogMode :: Maybe FileMode,
    -- | 'stripControl' for file logging.
    --
    -- @since 0.5
    fileLogStripControl :: !(Maybe StripControl),
    -- | Threshold for when we should warn about the log file size.
    --
    -- @since 0.5
    fileLogSizeMode :: !(Maybe FileSizeMode),
    -- | Whether to log commands.
    --
    -- @since 0.5
    cmdLogging :: !(Maybe CmdLogging),
    -- | Whether to display the command (key) names or the commands
    -- themselves.
    --
    -- @since 0.5
    cmdDisplay :: !(Maybe CmdDisplay),
    -- | The max number of command characters to display in the logs.
    --
    -- @since 0.5
    cmdNameTrunc :: !(Maybe (Truncation 'TCmdName)),
    -- | The max number of line characters to display in the logs.
    --
    -- @since 0.5
    cmdLineTrunc :: !(Maybe LineTruncation),
    -- | Determines to what extent we should remove control characters
    -- from logs.
    --
    -- @since 0.3
    stripControl :: !(Maybe StripControl),
    -- | Overarching option for logging. If it is false then all logging is
    -- disabled.
    --
    -- @since 0.5
    disableLogging :: !(Maybe Bool),
    -- | Legend text containing command aliases.
    --
    -- @since 0.5
    legend :: !(Maybe (List KeyVal))
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.5
makeFieldLabelsNoPrefix ''TomlConfig

-- | @since 0.5
instance Semigroup TomlConfig where
  MkTomlConfig a b c d e f g h i j k l <> MkTomlConfig a' b' c' d' e' f' g' h' i' j' k' l' =
    MkTomlConfig
      (a <|> a')
      (b <|> b')
      (c <|> c')
      (d <|> d')
      (e <|> e')
      (f <|> f')
      (g <|> g')
      (h <|> h')
      (i <|> i')
      (j <|> j')
      (k <|> k')
      (l <|> l')

-- | @since 0.5
instance Monoid TomlConfig where
  mempty =
    MkTomlConfig
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing

-- | @since 0.5
instance DecodeTOML TomlConfig where
  tomlDecoder =
    MkTomlConfig
      <$> decodeTimeout
      <*> decodeFileLogging
      <*> decodeFileLogMode
      <*> decodeFileLogStripControl
      <*> decodeFileLogSizeMode
      <*> decodeCmdLogging
      <*> decodeCmdDisplay
      <*> decodeCmdNameTrunc
      <*> decodeCmdLineTrunc
      <*> decodeStripControl
      <*> decodeDisableLogging
      <*> decodeLegend

-- | @since 0.5
decodeTimeout :: Decoder (Maybe Timeout)
decodeTimeout = getFieldOptWith tomlDecoder "timeout"

-- | @since 0.5
decodeFileLogging :: Decoder (Maybe FilePathDefault)
decodeFileLogging = getFieldOptWith tomlDecoder "file-log"

-- | @since 0.5
decodeFileLogMode :: Decoder (Maybe FileMode)
decodeFileLogMode = getFieldOptWith tomlDecoder "file-log-mode"

-- | @since 0.5
decodeCmdLogging :: Decoder (Maybe CmdLogging)
decodeCmdLogging = getFieldOptWith tomlDecoder "cmd-log"

-- | @since 0.5
decodeCmdDisplay :: Decoder (Maybe CmdDisplay)
decodeCmdDisplay = getFieldOptWith tomlDecoder "key-hide"

-- | @since 0.5
decodeCmdNameTrunc :: Decoder (Maybe (Truncation 'TCmdName))
decodeCmdNameTrunc = getFieldOptWith tomlDecoder "cmd-name-trunc"

-- | @since 0.5
decodeCmdLineTrunc :: Decoder (Maybe LineTruncation)
decodeCmdLineTrunc = getFieldOptWith tomlDecoder "cmd-line-trunc"

-- | @since 0.5
decodeStripControl :: Decoder (Maybe StripControl)
decodeStripControl = getFieldOptWith tomlDecoder "strip-control"

-- | @since 0.5
decodeFileLogStripControl :: Decoder (Maybe StripControl)
decodeFileLogStripControl = getFieldOptWith tomlDecoder "file-log-strip-control"

-- | @since 0.5
decodeFileLogSizeMode :: Decoder (Maybe FileSizeMode)
decodeFileLogSizeMode = getFieldOptWith tomlDecoder "file-log-size-mode"

-- | @since 0.5
decodeDisableLogging :: Decoder (Maybe Bool)
decodeDisableLogging = getFieldOptWith tomlDecoder "disable-log"

-- | @since 0.5
decodeLegend :: Decoder (Maybe (List KeyVal))
decodeLegend = getFieldOptWith tomlDecoder "legend"

-- | @since 0.5
argsToTomlConfig :: Getter Args TomlConfig
argsToTomlConfig = to a2c
  where
    a2c args =
      MkTomlConfig
        { timeout = args ^. #timeout,
          fileLogging = args ^. #fileLogging,
          fileLogMode = args ^. #fileLogMode,
          fileLogStripControl = args ^. #fileLogStripControl,
          fileLogSizeMode = args ^. #fileLogSizeMode,
          cmdLogging = args ^. #cmdLogging,
          cmdDisplay = args ^. #cmdDisplay,
          cmdNameTrunc = args ^. #cmdNameTrunc,
          cmdLineTrunc = args ^. #cmdLineTrunc,
          stripControl = args ^. #stripControl,
          disableLogging = args ^. #disableLogging,
          legend = Nothing
        }
