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
    WithDisable (Disabled, With),
    altDefault,
    defaultIfDisabled,
  )
import Shrun.Data.FileMode (FileMode (FileModeWrite))
import Shrun.Data.FilePathDefault (FilePathDefault)
import Shrun.Data.FileSizeMode (FileSizeMode, defaultFileSizeMode)
import Shrun.Data.StripControl (StripControl (StripControlAll))
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

-- File logging's path is only optional for the Args. For Toml and merged,
-- it must be present if file logging is active.
type FileLogPathF :: ConfigPhase -> Type
type family FileLogPathF p where
  FileLogPathF ConfigPhaseArgs = WithDisable (Maybe FilePathDefault)
  FileLogPathF ConfigPhaseToml = FilePathDefault
  FileLogPathF ConfigPhaseMerged = FilePathDefault

-- | Holds file logging config.
type FileLoggingP :: ConfigPhase -> Type
data FileLoggingP p = MkFileLoggingP
  { -- | Optional path to log file.
    path :: FileLogPathF p,
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
    With mPath -> case (mPath, mToml) of
      -- 2. Neither Args nor Toml specifies logging
      (Nothing, Nothing) -> Nothing
      -- 3. Args and no Toml
      (Just p, Nothing) ->
        Just
          $ MkFileLoggingP
            { path = p,
              stripControl =
                defaultIfDisabled StripControlAll (args ^. #stripControl),
              mode =
                defaultIfDisabled FileModeWrite (args ^. #mode),
              sizeMode =
                defaultIfDisabled defaultFileSizeMode (args ^. #sizeMode)
            }
      -- 4. Maybe Args and Toml
      (mArgsPath, Just toml) ->
        Just
          $ MkFileLoggingP
            { path = fromMaybe (toml ^. #path) mArgsPath,
              stripControl =
                altDefault' StripControlAll #stripControl (toml ^. #stripControl),
              mode =
                altDefault' FileModeWrite #mode (toml ^. #mode),
              sizeMode =
                altDefault' defaultFileSizeMode #sizeMode (toml ^. #sizeMode)
            }
  where
    altDefault' :: a -> Lens' FileLoggingArgs (WithDisable (Maybe a)) -> Maybe a -> a
    altDefault' defA = altDefault defA args

instance DecodeTOML FileLoggingToml where
  tomlDecoder =
    MkFileLoggingP
      <$> decodeFileLogging
      <*> decodeFileLogStripControl
      <*> decodeFileLogMode
      <*> decodeFileLogSizeMode

decodeFileLogging :: Decoder FilePathDefault
decodeFileLogging = getFieldWith tomlDecoder "path"

decodeFileLogStripControl :: Decoder (Maybe StripControl)
decodeFileLogStripControl = getFieldOptWith tomlDecoder "strip-control"

decodeFileLogMode :: Decoder (Maybe FileMode)
decodeFileLogMode = getFieldOptWith tomlDecoder "mode"

decodeFileLogSizeMode :: Decoder (Maybe FileSizeMode)
decodeFileLogSizeMode = getFieldOptWith tomlDecoder "size-mode"
