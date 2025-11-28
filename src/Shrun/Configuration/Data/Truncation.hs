{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.Truncation
  ( TruncRegion (..),
    Truncation (..),
    parseTruncation,
    LineTruncation (..),
    parseLineTruncation,

    -- * Misc
    decodeCommandNameTrunc,
    decodeLineTrunc,
    DetectResult (..),
    mergeLineTrunc,
    lineTruncStr,
  )
where

import Effects.System.Terminal (getTerminalWidth)
import Shrun.Configuration.Data.WithDisabled
  ( WithDisabled
      ( Disabled,
        With
      ),
  )
import Shrun.Prelude
import Shrun.Utils ((∸))
import Shrun.Utils qualified as Utils

-- | The different regions to apply truncation rules.
data TruncRegion
  = -- | Apply truncation rules to commands/key names.
    TruncCommandName
  | -- | Apply truncation rules to command log entire lines.
    TruncLine
  deriving stock (Eq, Show)

-- | The maximum number of command characters to display in the logs.
type Truncation :: TruncRegion -> Type
newtype Truncation a = MkTruncation
  { unTruncation :: Int
  }
  deriving stock (Eq, Ord, Show)
  deriving (Num) via Int

instance
  (k ~ An_Iso, a ~ Int, b ~ Int) =>
  LabelOptic "unTruncation" k (Truncation r) (Truncation r) a b
  where
  labelOptic = iso (\(MkTruncation x) -> x) MkTruncation
  {-# INLINE labelOptic #-}

instance DecodeTOML (Truncation a) where
  tomlDecoder = parseTruncation tomlDecoder

parseTruncation :: (MonadFail m) => m Natural -> m (Truncation r)
parseTruncation getNat = do
  n <- getNat
  case convertIntegral n of
    Left err -> fail err
    Right x -> pure $ MkTruncation x
{-# INLINEABLE parseTruncation #-}

-- | Determines command log line truncation behavior. We need a separate
-- type from 'Truncation' to add a third option, to detect the terminal size
-- automatically.
data LineTruncation
  = Undetected (Truncation TruncLine)
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
    -- NOTE: [Detect second parser]
    <|> parseDetected getTxt
{-# INLINEABLE parseLineTruncation #-}

-- Because this parser is used second, its error message is what will be
-- displayed.
--
-- see NOTE: [Detect second parser]
parseDetected :: (MonadFail m) => m Text -> m LineTruncation
parseDetected getTxt =
  getTxt >>= \case
    "detect" -> pure Detected
    bad ->
      fail
        $ Utils.fmtUnrecognizedError
          "line truncation"
          lineTruncStr
          (unpack bad)
{-# INLINEABLE parseDetected #-}

lineTruncStr :: String
lineTruncStr = "NATURAL | detect"

decodeCommandNameTrunc :: Decoder (Maybe (WithDisabled (Truncation TruncCommandName)))
decodeCommandNameTrunc = getFieldOptWith tomlDecoder "command-name-trunc"

decodeLineTrunc :: Decoder (Maybe (WithDisabled LineTruncation))
decodeLineTrunc = getFieldOptWith tomlDecoder "line-trunc"

data DetectResult
  = DetectNotRun
  | DetectFailed
  | DetectSucceeded Int

-- | Merges line truncation. Defaults to 'detect'.
mergeLineTrunc ::
  ( HasCallStack,
    MonadCatch m,
    MonadIORef m,
    MonadTerminal m
  ) =>
  IORef DetectResult ->
  Maybe (WithDisabled LineTruncation) ->
  Maybe (WithDisabled LineTruncation) ->
  m (Maybe (Truncation TruncLine))
mergeLineTrunc detectRef args toml = case args <|> toml of
  Nothing -> pure Nothing
  Just Disabled -> pure Nothing
  (Just (With t)) -> configToLineTrunc detectRef (Just t)

-- | Maps line trunc config to actual value.
configToLineTrunc ::
  ( HasCallStack,
    MonadCatch m,
    MonadIORef m,
    MonadTerminal m
  ) =>
  IORef DetectResult ->
  Maybe LineTruncation ->
  m (Maybe (Truncation TruncLine))
configToLineTrunc _ Nothing = pure Nothing
configToLineTrunc detectRef (Just Detected) = do
  -- ref exists so that we only check this once, even if both console
  -- and file specify detect.
  width <-
    readIORef detectRef >>= \case
      DetectSucceeded w -> pure w
      DetectFailed -> pure defLen
      DetectNotRun -> do
        -- We subtract one because otherwise we can fill the entire terminal with a
        -- log, which will automatically add a newline. The point of this option is
        -- to avoid multiple lines, hence the subtraction.
        tryMySync getTerminalWidth >>= \case
          Left ex -> do
            let msg =
                  mconcat
                    [ "Failed detecting terminal width, defaulting to 80: ",
                      displayExceptiont ex
                    ]
            putTextLn msg
            writeIORef detectRef DetectFailed
            pure defLen
          Right w -> pure $ w ∸ 1

  pure $ Just $ MkTruncation width
  where
    defLen = 80
configToLineTrunc _ (Just (Undetected x)) = pure $ Just x
{-# INLINEABLE configToLineTrunc #-}
