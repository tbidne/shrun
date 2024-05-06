{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.Truncation
  ( TruncRegion (..),
    Truncation (..),
    parseTruncation,
    LineTruncation (..),
    parseLineTruncation,

    -- * Misc
    decodeCmdNameTrunc,
    decodeLineTrunc,
    configToLineTrunc,
  )
where

import Effects.System.Terminal (getTerminalWidth)
import Shrun.Configuration.Data.WithDisabled (WithDisabled (..))
import Shrun.Prelude

-- | The different regions to apply truncation rules.
data TruncRegion
  = -- | Apply truncation rules to commands/key names.
    TCmdName
  | -- | Apply truncation rules to command log entire lines.
    TLine
  deriving stock (Eq, Show)

-- | The maximum number of command characters to display in the logs.
type Truncation :: TruncRegion -> Type
newtype Truncation a = MkTruncation
  { unTruncation :: Natural
  }
  deriving stock (Eq, Ord, Show)
  deriving (Num) via Natural

makeFieldLabelsNoPrefix ''Truncation

instance DecodeTOML (Truncation a) where
  tomlDecoder = parseTruncation tomlDecoder

parseTruncation :: (MonadFail m) => m Natural -> m (Truncation r)
parseTruncation getNat = MkTruncation <$> getNat

-- | Determines command log line truncation behavior. We need a separate
-- type from 'Truncation' to add a third option, to detect the terminal size
-- automatically.
data LineTruncation
  = Undetected (Truncation TLine)
  | Detected
  deriving stock (Eq, Show)

instance DecodeTOML LineTruncation where
  tomlDecoder = parseLineTruncation tomlDecoder tomlDecoder

parseLineTruncation ::
  (Alternative m, MonadFail m) =>
  m Natural ->
  m Text ->
  m LineTruncation
parseLineTruncation getNat getTxt =
  Undetected
    <$> parseTruncation getNat
    <|> parseDetected getTxt

parseDetected :: (MonadFail m) => m Text -> m LineTruncation
parseDetected getTxt =
  getTxt >>= \case
    "detect" -> pure Detected
    other -> fail $ "Wanted other, received: " <> unpack other

decodeCmdNameTrunc :: Decoder (Maybe (Truncation TCmdName))
decodeCmdNameTrunc = getFieldOptWith tomlDecoder "cmd-name-trunc"

decodeLineTrunc :: Decoder (Maybe LineTruncation)
decodeLineTrunc = getFieldOptWith tomlDecoder "line-trunc"

-- | Maps line trunc config to actual value.
configToLineTrunc ::
  ( HasCallStack,
    MonadTerminal m
  ) =>
  WithDisabled LineTruncation ->
  m (Maybe (Truncation TLine))
configToLineTrunc Disabled = pure Nothing
configToLineTrunc Without = pure Nothing
configToLineTrunc (With Detected) = Just . MkTruncation <$> getTerminalWidth
configToLineTrunc (With (Undetected x)) = pure $ Just x
