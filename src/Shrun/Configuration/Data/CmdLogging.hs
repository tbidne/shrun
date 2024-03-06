{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.CmdLogging
  ( CmdLoggingP (..),
    CmdLoggingArgs,
    CmdLoggingToml,
    CmdLoggingMerged,
    mergeCmdLogging,
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
    _MkWithDisable,
  )
import Shrun.Data.StripControl (StripControl (StripControlSmart))
import Shrun.Data.Truncation (LineTruncation)
import Shrun.Prelude

-- | Holds command logging config.
type CmdLoggingP :: ConfigPhase -> Type
data CmdLoggingP p = MkCmdLoggingP
  { stripControl :: ConfigPhaseF p StripControl,
    lineTrunc :: ConfigPhaseMaybeF p LineTruncation
  }

makeFieldLabelsNoPrefix ''CmdLoggingP

type CmdLoggingArgs = CmdLoggingP ConfigPhaseArgs

type CmdLoggingToml = CmdLoggingP ConfigPhaseToml

type CmdLoggingMerged = CmdLoggingP ConfigPhaseMerged

deriving stock instance Eq (CmdLoggingP ConfigPhaseArgs)

deriving stock instance Show (CmdLoggingP ConfigPhaseArgs)

deriving stock instance Eq (CmdLoggingP ConfigPhaseToml)

deriving stock instance Show (CmdLoggingP ConfigPhaseToml)

deriving stock instance Eq (CmdLoggingP ConfigPhaseMerged)

deriving stock instance Show (CmdLoggingP ConfigPhaseMerged)

-- | Merges args and toml configs.
mergeCmdLogging ::
  WithDisable Bool ->
  CmdLoggingArgs ->
  Maybe CmdLoggingToml ->
  Maybe CmdLoggingMerged
mergeCmdLogging withDisable args mToml =
  case (withDisable ^. _MkWithDisable, mToml) of
    -- 1. Logging globally disabled -> disable
    ((_, True), _) -> Nothing
    -- 2. Neither Args nor Toml specifies logging -> disable
    ((False, False), Nothing) -> Nothing
    -- 3. Args but no Toml -> Use Args
    ((True, False), Nothing) ->
      Just
        $ MkCmdLoggingP
          { stripControl = defaultIfDisabled StripControlSmart (args ^. #stripControl),
            lineTrunc = nothingIfDisabled (args ^. #lineTrunc)
          }
    -- 4. Maybe Args and Toml -> Merge (doesn't matter if Args specifies logging
    --    since we only need at least one of Args + Toml, and Toml does)
    (_, Just toml) ->
      Just
        $ MkCmdLoggingP
          { stripControl = altDefault' StripControlSmart #stripControl (toml ^. #stripControl),
            lineTrunc = altNothing' #lineTrunc (toml ^. #lineTrunc)
          }
  where
    altDefault' :: a -> Lens' CmdLoggingArgs (WithDisable (Maybe a)) -> Maybe a -> a
    altDefault' defA = altDefault defA args

    altNothing' :: Lens' CmdLoggingArgs (WithDisable (Maybe a)) -> Maybe a -> Maybe a
    altNothing' = altNothing args

instance DecodeTOML CmdLoggingToml where
  tomlDecoder =
    MkCmdLoggingP
      <$> decodeStripControl
      <*> decodeCmdLineTrunc

decodeStripControl :: Decoder (Maybe StripControl)
decodeStripControl = getFieldOptWith tomlDecoder "strip-control"

decodeCmdLineTrunc :: Decoder (Maybe LineTruncation)
decodeCmdLineTrunc = getFieldOptWith tomlDecoder "line-trunc"
