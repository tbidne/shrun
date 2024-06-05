{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.CommandLogging.ReadStrategy
  ( ReadStrategy (..),
    parseReadStrategy,
    readStrategyStr,
  )
where

import Data.String (IsString)
import Shrun.Prelude

-- | Different read strategies for simplicity vs. potential prettier
-- formatting.
data ReadStrategy
  = -- | Reads N bytes at a time.
    ReadBlock
  | -- | Reads N bytes at a time, but attempts to distinguish "complete" (newline
    -- terminated) vs. "partial" (anything else) reads. We do this to make
    -- the file log output prettier.
    ReadBlockLineBuffer
  deriving stock (Eq, Show)

instance DecodeTOML ReadStrategy where
  tomlDecoder = parseReadStrategy tomlDecoder

-- | Parses 'ReadStrategy'.
parseReadStrategy :: (MonadFail m) => m Text -> m ReadStrategy
parseReadStrategy getTxt =
  getTxt >>= \case
    "block" -> pure ReadBlock
    "block-line-buffer" -> pure ReadBlockLineBuffer
    other ->
      fail
        $ mconcat
          [ "Unrecognized read strategy: '",
            unpack other,
            "'. Expected one of ",
            readStrategyStr
          ]
{-# INLINEABLE parseReadStrategy #-}

-- | Available 'ReadStrategy' strings.
readStrategyStr :: (IsString a) => a
readStrategyStr = "(block | block-line-buffer)"
