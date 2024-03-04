{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'TomlConfig' type.
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
import Shrun.Configuration.Args (Args)
import Shrun.Data.FileMode (FileMode)
import Shrun.Data.FilePathDefault (FilePathDefault)
import Shrun.Data.FileSizeMode (FileSizeMode)
import Shrun.Data.KeyHide (KeyHide)
import Shrun.Data.Legend (KeyVal)
import Shrun.Data.PollInterval (PollInterval)
import Shrun.Data.StripControl (StripControl)
import Shrun.Data.Timeout (Timeout)
import Shrun.Data.TimerFormat (TimerFormat)
import Shrun.Data.Truncation (LineTruncation, TruncRegion (TCmdName), Truncation)
import Shrun.Notify.Types (NotifyAction, NotifySystemP1, NotifyTimeout)
import Shrun.Prelude

-- | Logging that applies to command logs.
data CmdLoggingToml = MkCmdLoggingToml
  { stripControl :: Maybe StripControl,
    lineTrunc :: Maybe LineTruncation
  }
  deriving stock (Eq, Show)

instance DecodeTOML CmdLoggingToml where
  tomlDecoder =
    MkCmdLoggingToml
      <$> decodeStripControl
      <*> decodeCmdLineTrunc

-- | Holds file logging data.
data FileLoggingToml = MkFileLoggingToml
  { path :: FilePathDefault,
    stripControl :: Maybe StripControl,
    mode :: Maybe FileMode,
    sizeMode :: Maybe FileSizeMode
  }
  deriving stock (Eq, Show)

instance DecodeTOML FileLoggingToml where
  tomlDecoder =
    MkFileLoggingToml
      <$> decodeFileLogging
      <*> decodeFileLogStripControl
      <*> decodeFileLogMode
      <*> decodeFileLogSizeMode

-- | Holds notification settings.
data NotifyToml = MkNotifyToml
  { -- | Notification action.
    action :: NotifyAction,
    -- | Notification system to use.
    system :: Maybe NotifySystemP1,
    -- | Timeout to use for notifications.
    timeout :: Maybe NotifyTimeout
  }
  deriving stock (Eq, Show)

instance DecodeTOML NotifyToml where
  tomlDecoder =
    MkNotifyToml
      <$> getFieldWith tomlDecoder "action"
      <*> getFieldOptWith tomlDecoder "system"
      <*> getFieldOptWith tomlDecoder "timeout"

-- | 'TomlConfig' refers to TomlConfiguration we retrieve from the toml TomlConfig file.
data TomlConfig = MkTomlConfig
  { -- | Timeout.
    timeout :: Maybe Timeout,
    -- | Shell logic to run before each command.
    init :: Maybe Text,
    -- | Whether to display the command (key) names or the commands
    -- themselves.
    keyHide :: Maybe KeyHide,
    -- | How often to poll commands for logs, in microseconds.
    pollInterval :: Maybe PollInterval,
    -- | Determines the max log size we read from commands in one go.
    cmdLogSize :: Maybe (Bytes B Natural),
    -- | How to format the timer.
    timerFormat :: Maybe TimerFormat,
    -- | Truncates command names in the logs.
    cmdNameTrunc :: Maybe (Truncation TCmdName),
    -- | Whether to log commands.
    cmdLog :: Maybe CmdLoggingToml,
    -- | Optional file logging. If enabled, holds the path to the file
    -- and the log queue.
    fileLog :: Maybe FileLoggingToml,
    -- | Holds notification toml config.
    notify :: Maybe NotifyToml,
    -- | Legend text containing command aliases.
    legend :: Maybe (List KeyVal)
  }
  deriving stock (Eq, Show)

-- | Returns an empty 'TomlConfig'.
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
    Nothing
    Nothing

instance DecodeTOML TomlConfig where
  tomlDecoder =
    MkTomlConfig
      <$> decodeTimeout
      <*> decodeInit
      <*> decodeCmdDisplay
      <*> decodePollInterval
      <*> decodeCmdLogSize
      <*> decodeTimerFormat
      <*> decodeCmdNameTrunc
      <*> getFieldOptWith tomlDecoder "cmd-log"
      <*> getFieldOptWith tomlDecoder "file-log"
      <*> getFieldOptWith tomlDecoder "notify"
      <*> decodeLegend

decodeTimeout :: Decoder (Maybe Timeout)
decodeTimeout = getFieldOptWith tomlDecoder "timeout"

decodeInit :: Decoder (Maybe Text)
decodeInit = getFieldOptWith tomlDecoder "init"

decodePollInterval :: Decoder (Maybe PollInterval)
decodePollInterval = getFieldOptWith tomlDecoder "poll-interval"

decodeTimerFormat :: Decoder (Maybe TimerFormat)
decodeTimerFormat = getFieldOptWith tomlDecoder "timer-format"

decodeFileLogging :: Decoder FilePathDefault
decodeFileLogging = getFieldWith tomlDecoder "path"

decodeFileLogMode :: Decoder (Maybe FileMode)
decodeFileLogMode = getFieldOptWith tomlDecoder "mode"

decodeCmdDisplay :: Decoder (Maybe KeyHide)
decodeCmdDisplay = getFieldOptWith tomlDecoder "key-hide"

decodeCmdNameTrunc :: Decoder (Maybe (Truncation TCmdName))
decodeCmdNameTrunc = getFieldOptWith tomlDecoder "cmd-name-trunc"

decodeCmdLogSize :: Decoder (Maybe (Bytes B Natural))
decodeCmdLogSize = getFieldOptWith (fmap MkBytes tomlDecoder) "cmd-log-size"

decodeCmdLineTrunc :: Decoder (Maybe LineTruncation)
decodeCmdLineTrunc = getFieldOptWith tomlDecoder "line-trunc"

decodeStripControl :: Decoder (Maybe StripControl)
decodeStripControl = getFieldOptWith tomlDecoder "strip-control"

decodeFileLogStripControl :: Decoder (Maybe StripControl)
decodeFileLogStripControl = getFieldOptWith tomlDecoder "strip-control"

decodeFileLogSizeMode :: Decoder (Maybe FileSizeMode)
decodeFileLogSizeMode = getFieldOptWith tomlDecoder "size-mode"

decodeLegend :: Decoder (Maybe (List KeyVal))
decodeLegend = getFieldOptWith tomlDecoder "legend"

makeFieldLabelsNoPrefix ''CmdLoggingToml

makeFieldLabelsNoPrefix ''FileLoggingToml

makeFieldLabelsNoPrefix ''TomlConfig

makeFieldLabelsNoPrefix ''NotifyToml

-- | Merges an 'Args' and 'TomlConfig' together to produce a single config.
-- In general, if both configurations specify a value, the CLI 'Args'
-- takes precedence.
mergeConfig :: Args -> TomlConfig -> TomlConfig
mergeConfig args tomlConfig =
  MkTomlConfig
    { timeout = combineWithDisable #timeout #timeout #noTimeout,
      init = combineWithDisable #init #init #noInit,
      keyHide = combineWithDisable #keyHide #keyHide #noKeyHide,
      pollInterval = combineWithDisable #pollInterval #pollInterval #noPollInterval,
      cmdLogSize = combineWithDisable #cmdLogSize #cmdLogSize #noCmdLogSize,
      timerFormat = combineWithDisable #timerFormat #timerFormat #noTimerFormat,
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
  Just
    $ MkCmdLoggingToml
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
  Just
    $ MkFileLoggingToml
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
  Just
    $ MkNotifyToml
      { action = fromMaybe (toml ^. #action) a,
        system = s <|> toml ^. #system,
        timeout = t <|> toml ^. #timeout
      }
