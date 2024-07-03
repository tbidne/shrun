{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.CommandLogging.ReadStrategy
  ( ReadStrategy (..),
    parseReadStrategy,
    readStrategyStr,
    defaultReadStrategy,
    readBlockLineBufferNotAllowed,
  )
where

import Data.String (IsString)
import Shrun.Data.Command (CommandP1)
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

defaultReadStrategy :: Bool -> NESeq CommandP1 -> ReadStrategy
defaultReadStrategy fileLogging cmds =
  if readBlockLineBufferNotAllowed fileLogging cmds
    then ReadBlock
    else ReadBlockLineBuffer

readBlockLineBufferNotAllowed :: Bool -> NESeq CommandP1 -> Bool
readBlockLineBufferNotAllowed fileLogging cmds =
  length cmds > 1 && fileLogging

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
