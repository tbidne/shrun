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
  )
import Shrun.Configuration.Data.WithDisabled
  ( WithDisabled (Disabled, With, Without),
    (<>?),
  )
import Shrun.Configuration.Data.WithDisabled qualified as WD
import Shrun.Data.StripControl (StripControl (StripControlSmart))
import Shrun.Data.Truncation
  ( LineTruncation (Detected, Undetected),
    TruncRegion (TCmdLine),
    Truncation (MkTruncation),
  )
import Shrun.Prelude

-- | Cmd log line truncation is truly optional, the default being none.
type CmdLogLineTruncF :: ConfigPhase -> Type
type family CmdLogLineTruncF p where
  CmdLogLineTruncF ConfigPhaseArgs = WithDisabled LineTruncation
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
  ( HasCallStack,
    MonadTerminal m
  ) =>
  WithDisabled () ->
  CmdLoggingArgs ->
  Maybe CmdLoggingToml ->
  m (Maybe CmdLoggingMerged)
mergeCmdLogging withDisabled args mToml =
  case withDisabled of
    -- 1. Logging globally disabled
    Disabled -> pure Nothing
    Without -> case mToml of
      -- 2. No Args and no Toml
      Nothing -> pure Nothing
      -- 3. No Args but yes Toml
      Just toml -> do
        cmdLogLineTrunc <-
          toLineTrunc $ view #lineTrunc args <>? view #lineTrunc toml

        pure
          $ Just
          $ MkCmdLoggingP
            { stripControl =
                plusDefault
                  StripControlSmart
                  #stripControl
                  (toml ^. #stripControl),
              lineTrunc = cmdLogLineTrunc
            }
    With _ -> case mToml of
      -- 4. Args but no Toml -> Use Args
      Nothing -> do
        cmdLogLineTrunc <- toLineTrunc (view #lineTrunc args)

        pure
          $ Just
          $ MkCmdLoggingP
            { stripControl =
                WD.fromWithDisabled
                  StripControlSmart
                  (view #stripControl args),
              lineTrunc = cmdLogLineTrunc
            }
      -- 5. Args and Toml -> Same as 3
      Just toml -> do
        cmdLogLineTrunc <-
          toLineTrunc $ view #lineTrunc args <>? view #lineTrunc toml

        pure
          $ Just
          $ MkCmdLoggingP
            { stripControl =
                plusDefault
                  StripControlSmart
                  #stripControl
                  (toml ^. #stripControl),
              lineTrunc = cmdLogLineTrunc
            }
  where
    plusDefault :: a -> Lens' CmdLoggingArgs (WithDisabled a) -> Maybe a -> a
    plusDefault defA l r = WD.fromWithDisabled defA $ (args ^. l) <>? r

instance DecodeTOML CmdLoggingToml where
  tomlDecoder =
    MkCmdLoggingP
      <$> decodeStripControl
      <*> decodeCmdLineTrunc

decodeStripControl :: Decoder (Maybe StripControl)
decodeStripControl = getFieldOptWith tomlDecoder "strip-control"

decodeCmdLineTrunc :: Decoder (Maybe LineTruncation)
decodeCmdLineTrunc = getFieldOptWith tomlDecoder "line-trunc"

toLineTrunc ::
  ( HasCallStack,
    MonadTerminal m
  ) =>
  WithDisabled LineTruncation ->
  m (Maybe (Truncation TCmdLine))
toLineTrunc Disabled = pure Nothing
toLineTrunc Without = pure Nothing
toLineTrunc (With Detected) = Just . MkTruncation <$> getTerminalWidth
toLineTrunc (With (Undetected x)) = pure $ Just x
