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

import Effects.System.Terminal (getTerminalWidth)
import Shrun.Configuration.Data.ConfigPhase
  ( ConfigPhase (ConfigPhaseArgs, ConfigPhaseMerged, ConfigPhaseToml),
    ConfigPhaseF,
    WithDisable (Disabled, With),
    altDefault,
    altNothing,
    defaultIfDisabled,
    nothingIfDisabled,
  )
import Shrun.Data.StripControl (StripControl (StripControlSmart))
import Shrun.Data.Truncation
  ( LineTruncation (Detected, Undetected),
    TruncRegion (TCmdLine),
    Truncation (MkTruncation),
  )
import Shrun.Prelude

-- Cmd log line truncation is truly optional, the default being none.
type CmdLogLineTruncF :: ConfigPhase -> Type
type family CmdLogLineTruncF p where
  CmdLogLineTruncF ConfigPhaseArgs = WithDisable (Maybe LineTruncation)
  CmdLogLineTruncF ConfigPhaseToml = Maybe LineTruncation
  CmdLogLineTruncF ConfigPhaseMerged = Maybe (Truncation TCmdLine)

-- | Holds command logging config.
type CmdLoggingP :: ConfigPhase -> Type
data CmdLoggingP p = MkCmdLoggingP
  { stripControl :: ConfigPhaseF p StripControl,
    lineTrunc :: CmdLogLineTruncF p
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
  ( MonadTerminal m
  ) =>
  WithDisable Bool ->
  CmdLoggingArgs ->
  Maybe CmdLoggingToml ->
  m (Maybe CmdLoggingMerged)
mergeCmdLogging withDisable args mToml =
  case withDisable of
    -- 1. Logging globally disabled
    Disabled -> pure Nothing
    With enabled -> case (enabled, mToml) of
      -- 2. Neither Args nor Toml specifies logging -> disable
      (False, Nothing) -> pure Nothing
      -- 3. Args but no Toml -> Use Args
      (True, Nothing) -> do
        cmdLogLineTrunc <- case nothingIfDisabled (args ^. #lineTrunc) of
          Just Detected -> Just . MkTruncation <$> getTerminalWidth
          Just (Undetected x) -> pure $ Just x
          Nothing -> pure Nothing

        pure
          $ Just
          $ MkCmdLoggingP
            { stripControl = defaultIfDisabled StripControlSmart (args ^. #stripControl),
              lineTrunc = cmdLogLineTrunc
            }
      -- 4. Maybe Args and Toml -> Merge (doesn't matter if Args specifies logging
      --    since we only need at least one of Args + Toml, and Toml does).
      --
      --    We combine toml w/ Args' config in altNothing/Default below.
      (_, Just toml) -> do
        cmdLogLineTrunc <- case altNothing' #lineTrunc (toml ^. #lineTrunc) of
          Just Detected -> Just . MkTruncation <$> getTerminalWidth
          Just (Undetected x) -> pure $ Just x
          Nothing -> pure Nothing

        pure
          $ Just
          $ MkCmdLoggingP
            { stripControl =
                altDefault'
                  StripControlSmart
                  #stripControl
                  (toml ^. #stripControl),
              lineTrunc = cmdLogLineTrunc
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
