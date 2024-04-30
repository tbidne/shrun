{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.ConsoleLogging
  ( ConsoleLoggingP (..),
    ConsoleLoggingArgs,
    ConsoleLoggingToml,
    ConsoleLoggingMerged,
    ConsoleLoggingEnv,
    mergeConsoleLogging,
    toEnv,
  )
where

import Effects.System.Terminal (getTerminalWidth)
import Shrun.Configuration.Data.ConfigPhase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseEnv,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
    ConfigPhaseF,
    ConfigPhaseMaybeF,
  )
import Shrun.Configuration.Data.WithDisabled
  ( WithDisabled (Disabled, With, Without),
    (<>?),
    _With,
  )
import Shrun.Configuration.Data.WithDisabled qualified as WD
import Shrun.Data.StripControl (StripControl, defaultConsoleLogStripControl)
import Shrun.Data.Truncation
  ( LineTruncation (Detected, Undetected),
    TruncRegion (TCmdName, TLine),
    Truncation (MkTruncation),
  )
import Shrun.Prelude

type CmdLoggingF :: ConfigPhase -> Type
type family CmdLoggingF p where
  CmdLoggingF ConfigPhaseArgs = WithDisabled ()
  CmdLoggingF ConfigPhaseToml = Maybe Bool
  CmdLoggingF ConfigPhaseMerged = Bool
  CmdLoggingF ConfigPhaseEnv = Bool

-- | Cmd log line truncation is truly optional, the default being none.
type CmdLogLineTruncF :: ConfigPhase -> Type
type family CmdLogLineTruncF p where
  CmdLogLineTruncF ConfigPhaseArgs = WithDisabled LineTruncation
  CmdLogLineTruncF ConfigPhaseToml = Maybe LineTruncation
  CmdLogLineTruncF ConfigPhaseMerged = Maybe (Truncation TLine)
  CmdLogLineTruncF ConfigPhaseEnv = Maybe (Truncation TLine)

-- | Holds command logging config.
type ConsoleLoggingP :: ConfigPhase -> Type
data ConsoleLoggingP p = MkConsoleLoggingP
  { cmdLogging :: CmdLoggingF p,
    cmdNameTrunc :: ConfigPhaseMaybeF p (Truncation TCmdName),
    lineTrunc :: CmdLogLineTruncF p,
    stripControl :: ConfigPhaseF p StripControl
  }

makeFieldLabelsNoPrefix ''ConsoleLoggingP

type ConsoleLoggingArgs = ConsoleLoggingP ConfigPhaseArgs

type ConsoleLoggingToml = ConsoleLoggingP ConfigPhaseToml

type ConsoleLoggingMerged = ConsoleLoggingP ConfigPhaseMerged

type ConsoleLoggingEnv = ConsoleLoggingP ConfigPhaseEnv

deriving stock instance Eq (ConsoleLoggingP ConfigPhaseArgs)

deriving stock instance Show (ConsoleLoggingP ConfigPhaseArgs)

deriving stock instance Eq (ConsoleLoggingP ConfigPhaseToml)

deriving stock instance Show (ConsoleLoggingP ConfigPhaseToml)

deriving stock instance Eq (ConsoleLoggingP ConfigPhaseMerged)

deriving stock instance Show (ConsoleLoggingP ConfigPhaseMerged)

-- | Merges args and toml configs.
mergeConsoleLogging ::
  ( HasCallStack,
    MonadTerminal m
  ) =>
  ConsoleLoggingArgs ->
  Maybe ConsoleLoggingToml ->
  m ConsoleLoggingMerged
mergeConsoleLogging args = \case
  Nothing -> do
    cmdLogLineTrunc <- toLineTrunc (view #lineTrunc args)

    pure
      $ MkConsoleLoggingP
        { cmdLogging = is (#cmdLogging % _With) args,
          cmdNameTrunc =
            WD.toMaybe (args ^. #cmdNameTrunc),
          lineTrunc = cmdLogLineTrunc,
          stripControl =
            WD.fromWithDisabled
              defaultConsoleLogStripControl
              (view #stripControl args)
        }
  Just toml -> do
    cmdLogLineTrunc <-
      toLineTrunc $ view #lineTrunc args <>? view #lineTrunc toml

    -- Convert WithDisabled () -> WithDisabled Bool for below operation.
    let argsCmdLogging :: WithDisabled Bool
        argsCmdLogging = args ^. #cmdLogging $> True

    pure
      $ MkConsoleLoggingP
        { cmdLogging =
            WD.fromWithDisabled
              False
              (argsCmdLogging <>? (toml ^. #cmdLogging)),
          cmdNameTrunc =
            plusNothing
              #cmdNameTrunc
              (toml ^. #cmdNameTrunc),
          lineTrunc = cmdLogLineTrunc,
          stripControl =
            plusDefault
              defaultConsoleLogStripControl
              #stripControl
              (toml ^. #stripControl)
        }
  where
    plusDefault :: a -> Lens' ConsoleLoggingArgs (WithDisabled a) -> Maybe a -> a
    plusDefault defA l r = WD.fromWithDisabled defA $ (args ^. l) <>? r

    plusNothing :: Lens' ConsoleLoggingArgs (WithDisabled a) -> Maybe a -> Maybe a
    plusNothing l r = WD.toMaybe $ (args ^. l) <>? r

instance DecodeTOML ConsoleLoggingToml where
  tomlDecoder =
    MkConsoleLoggingP
      <$> decodeCmdLogging
      <*> decodeCmdNameTrunc
      <*> decodeLineTrunc
      <*> decodeStripControl

decodeCmdLogging :: Decoder (Maybe Bool)
decodeCmdLogging = getFieldOptWith tomlDecoder "cmd"

decodeCmdNameTrunc :: Decoder (Maybe (Truncation TCmdName))
decodeCmdNameTrunc = getFieldOptWith tomlDecoder "cmd-name-trunc"

decodeLineTrunc :: Decoder (Maybe LineTruncation)
decodeLineTrunc = getFieldOptWith tomlDecoder "line-trunc"

decodeStripControl :: Decoder (Maybe StripControl)
decodeStripControl = getFieldOptWith tomlDecoder "strip-control"

toLineTrunc ::
  ( HasCallStack,
    MonadTerminal m
  ) =>
  WithDisabled LineTruncation ->
  m (Maybe (Truncation TLine))
toLineTrunc Disabled = pure Nothing
toLineTrunc Without = pure Nothing
toLineTrunc (With Detected) = Just . MkTruncation <$> getTerminalWidth
toLineTrunc (With (Undetected x)) = pure $ Just x

toEnv :: ConsoleLoggingMerged -> ConsoleLoggingEnv
toEnv merged =
  MkConsoleLoggingP
    { cmdLogging = merged ^. #cmdLogging,
      cmdNameTrunc = merged ^. #cmdNameTrunc,
      lineTrunc = merged ^. #lineTrunc,
      stripControl = merged ^. #stripControl
    }
