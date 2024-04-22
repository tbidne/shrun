{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.FileLogging
  ( FileLoggingP (..),
    FileLoggingArgs,
    FileLoggingToml,
    FileLoggingMerged,
    mergeFileLogging,
  )
where

import Shrun.Configuration.Data.ConfigPhase
  ( ConfigPhase (ConfigPhaseArgs, ConfigPhaseMerged, ConfigPhaseToml),
    ConfigPhaseF,
    ConfigPhaseMaybeF,
  )
import Shrun.Configuration.Data.WithDisabled
  ( WithDisabled (Disabled, With, Without),
    (<>?),
  )
import Shrun.Configuration.Data.WithDisabled qualified as WD
import Shrun.Data.FileMode (FileMode, defaultFileMode)
import Shrun.Data.FilePathDefault (FilePathDefault)
import Shrun.Data.FileSizeMode (FileSizeMode, defaultFileSizeMode)
import Shrun.Data.StripControl (StripControl, defaultFileLogStripControl)
import Shrun.Data.Truncation (TruncRegion (TCmdName), Truncation)
import Shrun.Prelude

-- NOTE: [Args vs. Toml mandatory fields]
--
-- Some fields are mandatory e.g. FileLogging's path if we are actually
-- doing file logging. The latter is determined by the FileLoggingP itself
-- being Just (cf. Nothing), thus the path itself is mandatory on Toml and
-- Merged.
--
-- So why is it optional on Args? Because Args' FileLoggingP is _always_
-- present, unlike Toml and Merged's Maybe. We need this behavior because the
-- former's fields can be used to override toml fields, even if file-logging is
-- not specified on the CLI.
--
-- For example, 'shrun --file-log-mode write cmd' _should_ overwrite toml's
-- file-log.mode even though we did not specify --file-log. Therefore Args'
-- FileLoggingP always needs to be present hence all its field must be
-- optional, even when some are mandatory on Merged.

-- | File logging's path is only optional for the Args. For Toml and merged,
-- it must be present if file logging is active.
type FileLogPathF :: ConfigPhase -> Type
type family FileLogPathF p where
  FileLogPathF ConfigPhaseArgs = WithDisabled FilePathDefault
  FileLogPathF ConfigPhaseToml = FilePathDefault
  FileLogPathF ConfigPhaseMerged = FilePathDefault

-- | Holds file logging config.
type FileLoggingP :: ConfigPhase -> Type
data FileLoggingP p = MkFileLoggingP
  { -- | Optional path to log file.
    path :: FileLogPathF p,
    -- | The max number of command characters to display in the file logs.
    cmdNameTrunc :: ConfigPhaseMaybeF p (Truncation TCmdName),
    -- | Determines to what extent we should remove control characters
    -- from file logs.
    stripControl :: ConfigPhaseF p StripControl,
    -- | Mode to use with the file log.
    mode :: ConfigPhaseF p FileMode,
    -- | Threshold for when we should warn about the log file size.
    sizeMode :: ConfigPhaseF p FileSizeMode
  }

makeFieldLabelsNoPrefix ''FileLoggingP

type FileLoggingArgs = FileLoggingP ConfigPhaseArgs

type FileLoggingToml = FileLoggingP ConfigPhaseToml

type FileLoggingMerged = FileLoggingP ConfigPhaseMerged

deriving stock instance Eq (FileLoggingP ConfigPhaseArgs)

deriving stock instance Show (FileLoggingP ConfigPhaseArgs)

deriving stock instance Eq (FileLoggingP ConfigPhaseToml)

deriving stock instance Show (FileLoggingP ConfigPhaseToml)

deriving stock instance Eq (FileLoggingP ConfigPhaseMerged)

deriving stock instance Show (FileLoggingP ConfigPhaseMerged)

-- | Merges args and toml configs.
mergeFileLogging ::
  FileLoggingArgs ->
  Maybe FileLoggingToml ->
  Maybe FileLoggingMerged
mergeFileLogging args mToml =
  case args ^. #path of
    -- 1. Logging globally disabled
    Disabled -> Nothing
    Without -> case mToml of
      -- 2. No Args and no Toml
      Nothing -> Nothing
      -- 3. No Args and yes Toml
      Just toml ->
        Just
          $ MkFileLoggingP
            { path = toml ^. #path,
              cmdNameTrunc =
                plusNothing
                  #cmdNameTrunc
                  (toml ^. #cmdNameTrunc),
              stripControl =
                plusDefault
                  defaultFileLogStripControl
                  #stripControl
                  (toml ^. #stripControl),
              mode =
                plusDefault
                  defaultFileMode
                  #mode
                  (toml ^. #mode),
              sizeMode =
                plusDefault
                  defaultFileSizeMode
                  #sizeMode
                  (toml ^. #sizeMode)
            }
    With path -> case mToml of
      -- 3. Yes Args and no Toml
      Nothing ->
        Just
          $ MkFileLoggingP
            { path,
              cmdNameTrunc =
                WD.toMaybe (args ^. #cmdNameTrunc),
              stripControl =
                WD.fromWithDisabled
                  defaultFileLogStripControl
                  (args ^. #stripControl),
              mode =
                WD.fromWithDisabled defaultFileMode (args ^. #mode),
              sizeMode =
                WD.fromWithDisabled defaultFileSizeMode (args ^. #sizeMode)
            }
      -- 4. Yes Args and yes Toml
      Just toml ->
        Just
          $ MkFileLoggingP
            { path,
              cmdNameTrunc =
                plusNothing
                  #cmdNameTrunc
                  (toml ^. #cmdNameTrunc),
              stripControl =
                plusDefault
                  defaultFileLogStripControl
                  #stripControl
                  (view #stripControl toml),
              mode =
                plusDefault
                  defaultFileMode
                  #mode
                  (view #mode toml),
              sizeMode =
                plusDefault
                  defaultFileSizeMode
                  #sizeMode
                  (view #sizeMode toml)
            }
  where
    plusDefault :: a -> Lens' FileLoggingArgs (WithDisabled a) -> Maybe a -> a
    plusDefault defA l r = WD.fromWithDisabled defA $ (args ^. l) <>? r

    plusNothing :: Lens' FileLoggingArgs (WithDisabled a) -> Maybe a -> Maybe a
    plusNothing l r = WD.toMaybe $ (args ^. l) <>? r

instance DecodeTOML FileLoggingToml where
  tomlDecoder =
    MkFileLoggingP
      <$> decodeFileLogging
      <*> decodeFileCmdNameTrunc
      <*> decodeFileLogStripControl
      <*> decodeFileLogMode
      <*> decodeFileLogSizeMode

decodeFileLogging :: Decoder FilePathDefault
decodeFileLogging = getFieldWith tomlDecoder "path"

decodeFileCmdNameTrunc :: Decoder (Maybe (Truncation TCmdName))
decodeFileCmdNameTrunc = getFieldOptWith tomlDecoder "cmd-name-trunc"

decodeFileLogStripControl :: Decoder (Maybe StripControl)
decodeFileLogStripControl = getFieldOptWith tomlDecoder "strip-control"

decodeFileLogMode :: Decoder (Maybe FileMode)
decodeFileLogMode = getFieldOptWith tomlDecoder "mode"

decodeFileLogSizeMode :: Decoder (Maybe FileSizeMode)
decodeFileLogSizeMode = getFieldOptWith tomlDecoder "size-mode"
