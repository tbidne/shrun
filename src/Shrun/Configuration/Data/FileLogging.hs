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
    WithDisable,
    altDefault,
    altNothing,
    defaultIfDisabled,
    nothingIfDisabled,
    _DisableA,
    _DisableBool,
  )
import Shrun.Data.FileMode (FileMode (FileModeWrite))
import Shrun.Data.FilePathDefault (FilePathDefault)
import Shrun.Data.FileSizeMode (FileSizeMode)
import Shrun.Data.StripControl (StripControl (StripControlAll, StripControlSmart))
import Shrun.Prelude

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
    sizeMode :: ConfigPhaseMaybeF p FileSizeMode
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
mergeFileLogging cli mToml =
  if cli ^. (#path % _DisableBool)
    then -- 1. Logging globally disabled
      Nothing
    else case (cli ^. (#path % _DisableA), mToml) of
      -- 2. Neither CLI nor Toml specifies logging
      (Nothing, Nothing) -> Nothing
      -- 3. CLI and no Toml
      (Just p, Nothing) ->
        Just
          $ MkFileLoggingP
            { path = p,
              stripControl =
                defaultIfDisabled StripControlSmart (cli ^. #stripControl),
              mode =
                defaultIfDisabled FileModeWrite (cli ^. #mode),
              sizeMode = nothingIfDisabled (cli ^. #sizeMode)
            }
      -- 4. Maybe CLI and Toml
      (mArgsPath, Just toml) ->
        Just
          $ MkFileLoggingP
            { path = fromMaybe (toml ^. #path) mArgsPath,
              stripControl =
                altDefault' StripControlAll #stripControl (toml ^. #stripControl),
              mode =
                altDefault' FileModeWrite #mode (toml ^. #mode),
              sizeMode = altNothing' #sizeMode (toml ^. #sizeMode)
            }
  where
    altDefault' :: a -> Lens' FileLoggingArgs (WithDisable (Maybe a)) -> Maybe a -> a
    altDefault' defA = altDefault defA cli

    altNothing' :: Lens' FileLoggingArgs (WithDisable (Maybe a)) -> Maybe a -> Maybe a
    altNothing' = altNothing cli

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
