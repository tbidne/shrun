{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'TomlConfig' type.
--
-- @since 0.5
module Shrun.Configuration.Toml
  ( TomlConfig (..),
    defaultTomlConfig,
    CmdLoggingToml (..),
    FileLoggingToml (..),
    NotifyToml (..),
    mergeConfig,
  )
where

import Optics.Core (NoIx)
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
import Shrun.Data.PollInterval (PollInterval)
import Shrun.Data.Timeout (Timeout)
import Shrun.Notify.Types (NotifyAction, NotifySystemP1, NotifyTimeout)
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
instance DecodeTOML FileLoggingToml where
  tomlDecoder =
    MkFileLoggingToml
      <$> decodeFileLogging
      <*> decodeFileLogStripControl
      <*> decodeFileLogMode
      <*> decodeFileLogSizeMode

-- | Holds notification settings.
--
-- @since X.X
data NotifyToml = MkNotifyToml
  { -- | Notification action.
    --
    -- @since X.X
    action :: !NotifyAction,
    -- | Notification system to use.
    --
    -- @since X.X
    system :: !(Maybe NotifySystemP1),
    -- | Timeout to use for notifications.
    --
    -- @since X.X
    timeout :: !(Maybe NotifyTimeout)
  }
  deriving stock
    ( -- | @since X.X
      Eq,
      -- | @since X.X
      Show
    )

-- | @since X.X
instance DecodeTOML NotifyToml where
  tomlDecoder =
    MkNotifyToml
      <$> getFieldWith tomlDecoder "action"
      <*> getFieldOptWith tomlDecoder "system"
      <*> getFieldOptWith tomlDecoder "timeout"

-- | 'TomlConfig' refers to TomlConfiguration we retrieve from the toml TomlConfig file.
--
-- @since 0.5
data TomlConfig = MkTomlConfig
  { -- | Timeout.
    --
    -- @since 0.5
    timeout :: !(Maybe Timeout),
    -- | Shell logic to run before each command.
    --
    -- @since 0.8
    init :: !(Maybe Text),
    -- | Whether to display the command (key) names or the commands
    -- themselves.
    --
    -- @since 0.5
    keyHide :: !(Maybe CmdDisplay),
    -- | How often to poll commands for logs, in microseconds.
    --
    -- @since 0.8
    pollInterval :: !(Maybe PollInterval),
    -- | Truncates command names in the logs.
    --
    -- @since 0.6
    cmdNameTrunc :: !(Maybe (Truncation TCmdName)),
    -- | Whether to log commands.
    --
    -- @since 0.6
    cmdLog :: !(Maybe CmdLoggingToml),
    -- | Optional file logging. If enabled, holds the path to the file
    -- and the log queue.
    --
    -- @since 0.6
    fileLog :: !(Maybe FileLoggingToml),
    -- | Holds notification toml config.
    --
    -- @since X.X
    notify :: !(Maybe NotifyToml),
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

-- | Returns an empty 'TomlConfig'.
--
-- @since 0.7
defaultTomlConfig :: TomlConfig
defaultTomlConfig =
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
      <*> decodeInit
      <*> decodeCmdDisplay
      <*> decodePollInterval
      <*> decodeCmdNameTrunc
      <*> getFieldOptWith tomlDecoder "cmd-log"
      <*> getFieldOptWith tomlDecoder "file-log"
      <*> getFieldOptWith tomlDecoder "notify"
      <*> decodeLegend

-- | @since 0.5
decodeTimeout :: Decoder (Maybe Timeout)
decodeTimeout = getFieldOptWith tomlDecoder "timeout"

-- | @since 0.8
decodeInit :: Decoder (Maybe Text)
decodeInit = getFieldOptWith tomlDecoder "init"

-- | @since 0.8
decodePollInterval :: Decoder (Maybe PollInterval)
decodePollInterval = getFieldOptWith tomlDecoder "poll-interval"

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
decodeCmdNameTrunc :: Decoder (Maybe (Truncation TCmdName))
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
decodeLegend :: Decoder (Maybe (List KeyVal))
decodeLegend = getFieldOptWith tomlDecoder "legend"

-- | @since 0.6
makeFieldLabelsNoPrefix ''CmdLoggingToml

-- | @since 0.6
makeFieldLabelsNoPrefix ''FileLoggingToml

-- | @since 0.5
makeFieldLabelsNoPrefix ''TomlConfig

-- | @since X.X
makeFieldLabelsNoPrefix ''NotifyToml

-- | Merges an 'Args' and 'TomlConfig' together to produce a single config.
-- In general, if both configurations specify a value, the CLI 'Args'
-- takes precedence.
--
-- @since 0.1
mergeConfig :: Args -> TomlConfig -> TomlConfig
mergeConfig args tomlConfig =
  MkTomlConfig
    { timeout = combineWithDisable #timeout #timeout #noTimeout,
      init = combineWithDisable #init #init #noInit,
      keyHide = combineWithDisable #keyHide #keyHide #noKeyHide,
      pollInterval = combineWithDisable #pollInterval #pollInterval #noPollInterval,
      cmdNameTrunc = combineWithDisable #cmdNameTrunc #cmdNameTrunc #noCmdNameTrunc,
      cmdLog,
      fileLog,
      notify,
      legend = tomlConfig ^. #legend
    }
  where
    -- Combine LHS args and RHS toml, with the disable flag taking priority
    combineWithDisable ::
      ( Alternative f,
        Is k A_Getter
      ) =>
      Optic' k NoIx Args (f b) ->
      Optic' k NoIx TomlConfig (f b) ->
      Optic' k NoIx Args Bool ->
      f b
    combineWithDisable argsGetter tomlGetter argsDisable
      | args ^. argsDisable = empty
      | otherwise = args ^. argsGetter <|> tomlConfig ^. tomlGetter

    -- Either return the parameter or empty, depending on the disable flag
    disabledOrResult ::
      ( Alternative f,
        Is k A_Getter
      ) =>
      Optic' k NoIx Args Bool ->
      f b ->
      f b
    disabledOrResult argsDisable y
      | args ^. argsDisable = empty
      | otherwise = y

    -- NOTE: The logic here is as follows:
    --
    -- 1. Combine LHS (args) and RHS (toml) into result r: i.e. args taking
    --    priority.
    -- 2. For each field, disable if the corresponding args disable field is
    --    active. Otherwise take r.

    argsCmdLog =
      ( args ^. #cmdLog,
        args ^. #cmdLogStripControl,
        args ^. #cmdLogLineTrunc
      )
    cmdLog =
      -- noCmdLog overrides all cmd logging
      if args ^. #noCmdLog
        then Nothing
        else -- cmdLog might be active, combine args and toml and selectively
        -- disable fields as necessary

          combineCmdLog argsCmdLog (tomlConfig ^. #cmdLog) <&> \y ->
            MkCmdLoggingToml
              { stripControl = disabledOrResult #noCmdLogStripControl (y ^. #stripControl),
                lineTrunc = disabledOrResult #noCmdLogLineTrunc (y ^. #lineTrunc)
              }

    argsFileLog =
      ( args ^. #fileLog,
        args ^. #fileLogStripControl,
        args ^. #fileLogMode,
        args ^. #fileLogSizeMode
      )
    fileLog =
      if args ^. #noFileLog
        then Nothing
        else
          combineFileLog argsFileLog (tomlConfig ^. #fileLog) <&> \y ->
            MkFileLoggingToml
              { path = y ^. #path,
                stripControl = disabledOrResult #noFileLogStripControl (y ^. #stripControl),
                mode = disabledOrResult #noFileLogMode (y ^. #mode),
                sizeMode = disabledOrResult #noFileLogSizeMode (y ^. #sizeMode)
              }

    argsNotify =
      ( args ^. #notifyAction,
        args ^. #notifySystem,
        args ^. #notifyTimeout
      )
    notify =
      if args ^. #noNotifyAction
        then Nothing
        else
          combineNotify argsNotify (tomlConfig ^. #notify) <&> \y ->
            MkNotifyToml
              { action = y ^. #action,
                system = disabledOrResult #noNotifySystem (y ^. #system),
                timeout = disabledOrResult #noNotifyTimeout (y ^. #timeout)
              }

combineCmdLog ::
  -- | Args
  (Maybe Bool, Maybe StripControl, Maybe LineTruncation) ->
  -- | Toml
  Maybe CmdLoggingToml ->
  -- | Result
  Maybe CmdLoggingToml
-- 1. If neither CLI nor toml specifies cmd logging, return no logging
combineCmdLog (Just False, _, _) Nothing = Nothing
combineCmdLog (Nothing, _, _) Nothing = Nothing
-- 2. If only the CLI specifies cmd logging, use its config
combineCmdLog (Just True, sc, lt) Nothing = Just $ MkCmdLoggingToml sc lt
-- 3. If toml specifies cmd logging, combine args, favoring CLI as usual
combineCmdLog (_, mStripControl, mlineTrunc) (Just toml) =
  Just $
    MkCmdLoggingToml
      { stripControl = mStripControl <|> toml ^. #stripControl,
        lineTrunc = mlineTrunc <|> toml ^. #lineTrunc
      }

combineFileLog ::
  -- | Args
  (Maybe FilePathDefault, Maybe StripControl, Maybe FileMode, Maybe FileSizeMode) ->
  -- | Toml
  Maybe FileLoggingToml ->
  -- | Result
  Maybe FileLoggingToml
-- 1. If neither CLI nor toml specifies file logging, return no logging
combineFileLog (Nothing, _, _, _) Nothing = Nothing
-- 2. If only the CLI specifies file logging, use its config
combineFileLog (Just f, sc, m, sm) Nothing = Just $ MkFileLoggingToml f sc m sm
-- 3. If toml specifies file logging, combine args, favoring CLI as usual
combineFileLog (mpath, mStripControl, mMode, mSizeMode) (Just toml) =
  Just $
    MkFileLoggingToml
      { path = fromMaybe (toml ^. #path) mpath,
        stripControl = mStripControl <|> toml ^. #stripControl,
        mode = mMode <|> toml ^. #mode,
        sizeMode = mSizeMode <|> toml ^. #sizeMode
      }

combineNotify ::
  -- | Args
  (Maybe NotifyAction, Maybe NotifySystemP1, Maybe NotifyTimeout) ->
  -- | Toml
  Maybe NotifyToml ->
  -- | Result
  Maybe NotifyToml
-- 1. If neither CLI nor toml specifies the action, return nothing
combineNotify (Nothing, _, _) Nothing = Nothing
-- 2. If only the CLI specifies toml, use its config
combineNotify (Just a, s, t) Nothing = Just $ MkNotifyToml a s t
-- 3. If toml specifies notify, combine args, favoring CLI as usual
combineNotify (a, s, t) (Just toml) =
  Just $
    MkNotifyToml
      { action = fromMaybe (toml ^. #action) a,
        system = s <|> toml ^. #system,
        timeout = t <|> toml ^. #timeout
      }
