{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'TomlConfig' type.
--
-- @since 0.5
module ShellRun.Configuration.Toml
  ( TomlConfig (..),
    argsToTomlConfig,
  )
where

import ShellRun.Configuration.Args (Args (..))
import ShellRun.Configuration.Env.Types
  ( CmdDisplay,
    CmdLogging,
    LineTruncation,
    StripControl,
    TruncRegion (..),
    Truncation,
  )
import ShellRun.Data.FilePathDefault (FilePathDefault)
import ShellRun.Data.Timeout (Timeout)
import ShellRun.Prelude

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
    legend :: !(Maybe Text)
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
  MkTomlConfig a b c d e f g h i <> MkTomlConfig a' b' c' d' e' f' g' h' i' =
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

-- | @since 0.5
instance DecodeTOML TomlConfig where
  tomlDecoder =
    MkTomlConfig
      <$> decodeTimeout
      <*> decodeFileLogging
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
decodeFileLogging = getFieldOptWith tomlDecoder "file-logging"

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
decodeDisableLogging :: Decoder (Maybe Bool)
decodeDisableLogging = getFieldOptWith tomlDecoder "disable-log"

-- | @since 0.5
decodeLegend :: Decoder (Maybe Text)
decodeLegend = getFieldOptWith tomlDecoder "legend"

-- | @since 0.5
argsToTomlConfig :: Getter Args TomlConfig
argsToTomlConfig = to a2c
  where
    a2c args =
      MkTomlConfig
        { timeout = args ^. #timeout,
          fileLogging = args ^. #fileLogging,
          cmdLogging = args ^. #cmdLogging,
          cmdDisplay = args ^. #cmdDisplay,
          cmdNameTrunc = args ^. #cmdNameTrunc,
          cmdLineTrunc = args ^. #cmdLineTrunc,
          stripControl = args ^. #stripControl,
          disableLogging = args ^. #disableLogging,
          legend = Nothing
        }
