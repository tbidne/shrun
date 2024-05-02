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

import Shrun.Configuration.Data.ConfigPhase
  ( BoolF,
    ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseEnv,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
    ConfigPhaseF,
    ConfigPhaseMaybeF,
    LineTruncF,
  )
import Shrun.Configuration.Data.WithDisabled
  ( WithDisabled,
    (<>?),
    _With,
  )
import Shrun.Configuration.Data.WithDisabled qualified as WD
import Shrun.Data.StripControl (StripControl, defaultConsoleLogStripControl)
import Shrun.Data.Truncation
  ( TruncRegion (TCmdName),
    Truncation,
    configToLineTrunc,
    decodeCmdNameTrunc,
    decodeLineTrunc,
  )
import Shrun.Prelude

-- | Holds command logging config.
type ConsoleLoggingP :: ConfigPhase -> Type
data ConsoleLoggingP p = MkConsoleLoggingP
  { cmdLogging :: BoolF p,
    cmdNameTrunc :: ConfigPhaseMaybeF p (Truncation TCmdName),
    lineTrunc :: LineTruncF p,
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
    lineTrunc <- configToLineTrunc (args ^. #lineTrunc)

    pure
      $ MkConsoleLoggingP
        { cmdLogging = is (#cmdLogging % _With) args,
          cmdNameTrunc =
            WD.toMaybe (args ^. #cmdNameTrunc),
          lineTrunc,
          stripControl =
            WD.fromWithDisabled
              defaultConsoleLogStripControl
              (view #stripControl args)
        }
  Just toml -> do
    lineTrunc <-
      configToLineTrunc $ view #lineTrunc args <>? view #lineTrunc toml

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
          lineTrunc,
          stripControl =
            plusDefault
              defaultConsoleLogStripControl
              #stripControl
              (toml ^. #stripControl)
        }
  where
    -- Convert WithDisabled () -> WithDisabled Bool for below operation.
    argsCmdLogging :: WithDisabled Bool
    argsCmdLogging = args ^. #cmdLogging $> True

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

decodeStripControl :: Decoder (Maybe StripControl)
decodeStripControl = getFieldOptWith tomlDecoder "strip-control"

toEnv :: ConsoleLoggingMerged -> ConsoleLoggingEnv
toEnv merged =
  MkConsoleLoggingP
    { cmdLogging = merged ^. #cmdLogging,
      cmdNameTrunc = merged ^. #cmdNameTrunc,
      lineTrunc = merged ^. #lineTrunc,
      stripControl = merged ^. #stripControl
    }
