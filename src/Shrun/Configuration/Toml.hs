{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'TomlConfig' type.
--
-- @since 0.5
module Shrun.Configuration.Toml
  ( TomlConfig (..),
    CmdLoggingToml (..),
    FileLoggingToml (..),
    argsToTomlConfig,
  )
where

import Shrun.Configuration.Args (Args (..), FileMode (..), FileSizeMode (..))
import Shrun.Configuration.Env.Types
  ( CmdDisplay,
    LineTruncation,
    StripControl,
    TruncRegion (..),
    Truncation,
  )
import Shrun.Data.FilePathDefault (FilePathDefault)
import Shrun.Data.Legend (KeyVal)
import Shrun.Data.Timeout (Timeout)
import Shrun.Prelude

-- | Logging that applies to command logs.
--
-- @since 0.6
data CmdLoggingToml = MkCmdLoggingToml
  { -- | @since 0.6
    stripControl :: !(Maybe StripControl),
    -- | @since 0.6
    lineTrunc :: !(Maybe LineTruncation)
  }
  deriving stock
    ( -- | @since 0.6
      Eq,
      -- | @since 0.6
      Show
    )

-- | @since 0.6
instance Semigroup CmdLoggingToml where
  MkCmdLoggingToml a b <> MkCmdLoggingToml a' b' =
    MkCmdLoggingToml (a <|> a') (b <|> b')

-- | @since 0.6
instance Monoid CmdLoggingToml where
  mempty = MkCmdLoggingToml empty empty

-- | @since 0.6
instance DecodeTOML CmdLoggingToml where
  tomlDecoder =
    MkCmdLoggingToml
      <$> decodeStripControl
      <*> decodeCmdLineTrunc

-- | @since 0.6
data FileLoggingToml = MkFileLoggingToml
  { -- | @since 0.6
    path :: !FilePathDefault,
    -- | @since 0.6
    stripControl :: !(Maybe StripControl),
    -- | @since 0.6
    mode :: !(Maybe FileMode),
    -- | @since 0.6
    sizeMode :: !(Maybe FileSizeMode)
  }
  deriving stock
    ( -- | @since 0.6
      Eq,
      -- | @since 0.6
      Show
    )

-- | @since 0.6
instance Semigroup FileLoggingToml where
  -- NOTE: for the path, always take the LHS for consistency w/ other
  -- Alternatives.
  MkFileLoggingToml a b c d <> MkFileLoggingToml _ b' c' d' =
    MkFileLoggingToml a (b <|> b') (c <|> c') (d <|> d')

-- | @since 0.6
instance DecodeTOML FileLoggingToml where
  tomlDecoder =
    MkFileLoggingToml
      <$> decodeFileLogging
      <*> decodeFileLogStripControl
      <*> decodeFileLogMode
      <*> decodeFileLogSizeMode

-- | 'TomlConfig' refers to TomlConfiguration we retrieve from the toml TomlConfig file.
--
-- @since 0.5
data TomlConfig = MkTomlConfig
  { -- | Timeout.
    --
    -- @since 0.5
    timeout :: !(Maybe Timeout),
    -- | Overarching option for logging. If it is false then all logging is
    -- disabled.
    --
    -- @since 0.5
    disableLogging :: !(Maybe Bool),
    -- | Whether to display the command (key) names or the commands
    -- themselves.
    --
    -- @since 0.5
    cmdDisplay :: !(Maybe CmdDisplay),
    -- | Truncates command names in the logs.
    --
    -- @since 0.6
    cmdNameTrunc :: !(Maybe (Truncation 'TCmdName)),
    -- | Whether to log commands.
    --
    -- @since 0.6
    cmdLogging :: !(Maybe CmdLoggingToml),
    -- | Optional file logging. If enabled, holds the path to the file
    -- and the log queue.
    --
    -- @since 0.6
    fileLogging :: !(Maybe FileLoggingToml),
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
instance Semigroup TomlConfig where
  MkTomlConfig a b c d e f g <> MkTomlConfig a' b' c' d' e' f' g' =
    MkTomlConfig
      (a <|> a')
      (b <|> b')
      (c <|> c')
      (d <|> d')
      -- NOTE: For aggregate types (i.e. cmd/file logging), use semigroup
      -- instance since we want decisions at the individual _field_ level.
      (e <> e')
      (f <> f')
      (g <|> g')

-- | @since 0.5
instance Monoid TomlConfig where
  mempty =
    MkTomlConfig
      empty
      empty
      empty
      empty
      mempty
      mempty
      empty

-- | @since 0.5
instance DecodeTOML TomlConfig where
  tomlDecoder =
    MkTomlConfig
      <$> decodeTimeout
      <*> decodeDisableLogging
      <*> decodeCmdDisplay
      <*> decodeCmdNameTrunc
      <*> getFieldOptWith tomlDecoder "cmd-log"
      <*> getFieldOptWith tomlDecoder "file-log"
      <*> decodeLegend

-- | @since 0.5
decodeTimeout :: Decoder (Maybe Timeout)
decodeTimeout = getFieldOptWith tomlDecoder "timeout"

-- | @since 0.5
decodeFileLogging :: Decoder FilePathDefault
decodeFileLogging = getFieldWith tomlDecoder "path"

-- | @since 0.5
decodeFileLogMode :: Decoder (Maybe FileMode)
decodeFileLogMode = getFieldOptWith tomlDecoder "mode"

-- | @since 0.5
decodeCmdDisplay :: Decoder (Maybe CmdDisplay)
decodeCmdDisplay = getFieldOptWith tomlDecoder "key-hide"

-- | @since 0.5
decodeCmdNameTrunc :: Decoder (Maybe (Truncation 'TCmdName))
decodeCmdNameTrunc = getFieldOptWith tomlDecoder "cmd-name-trunc"

-- | @since 0.5
decodeCmdLineTrunc :: Decoder (Maybe LineTruncation)
decodeCmdLineTrunc = getFieldOptWith tomlDecoder "line-trunc"

-- | @since 0.5
decodeStripControl :: Decoder (Maybe StripControl)
decodeStripControl = getFieldOptWith tomlDecoder "strip-control"

-- | @since 0.5
decodeFileLogStripControl :: Decoder (Maybe StripControl)
decodeFileLogStripControl = getFieldOptWith tomlDecoder "strip-control"

-- | @since 0.5
decodeFileLogSizeMode :: Decoder (Maybe FileSizeMode)
decodeFileLogSizeMode = getFieldOptWith tomlDecoder "size-mode"

-- | @since 0.5
decodeDisableLogging :: Decoder (Maybe Bool)
decodeDisableLogging = getFieldOptWith tomlDecoder "log-disable"

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
          disableLogging = args ^. #disableLogging,
          cmdDisplay = args ^. #cmdDisplay,
          cmdNameTrunc = args ^. #cmdNameTrunc,
          cmdLogging = case args ^. #cmdLogging of
            Just True ->
              Just
                MkCmdLoggingToml
                  { stripControl = args ^. #cmdLogStripControl,
                    lineTrunc = args ^. #cmdLogLineTrunc
                  }
            _ -> Nothing,
          fileLogging = case args ^. #fileLogging of
            Just f ->
              Just
                MkFileLoggingToml
                  { path = f,
                    mode = args ^. #fileLogMode,
                    stripControl = args ^. #fileLogStripControl,
                    sizeMode = args ^. #fileLogSizeMode
                  }
            _ -> Nothing,
          legend = Nothing
        }

-- | @since 0.6
makeFieldLabelsNoPrefix ''CmdLoggingToml

-- | @since 0.6
makeFieldLabelsNoPrefix ''FileLoggingToml

-- | @since 0.5
makeFieldLabelsNoPrefix ''TomlConfig
