{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.CommandLogging.ReadStrategy
  ( ReadStrategy (..),
    parseReadStrategy,
    readStrategyMeta,
    defaultReadStrategy,
    readBlockLineBufferNotAllowed,
  )
where

import Shrun.Command.Types (CommandP1)
import Shrun.Prelude
import Shrun.Utils qualified as Utils

-- | Different read strategies for simplicity vs. potential prettier
-- formatting.
data ReadStrategy
  = -- | Reads N bytes at a time.
    ReadBlock
  | -- | Reads N bytes at a time, but attempts to distinguish "complete" (newline
    -- terminated) vs. "partial" (anything else) reads. We do this to make
    -- the file log output prettier.
    ReadBlockLineBuffer
  deriving stock (Bounded, Enum, Eq, Show)

instance DecodeTOML ReadStrategy where
  tomlDecoder = parseReadStrategy tomlDecoder

instance Pretty ReadStrategy where
  pretty = \case
    ReadBlock -> "block"
    ReadBlockLineBuffer -> "block-line-buffer"

defaultReadStrategy :: Bool -> Bool -> NESeq CommandP1 -> ReadStrategy
defaultReadStrategy isFileLog isFileLogMulti cmds =
  if readBlockLineBufferNotAllowed isFileLog isFileLogMulti cmds
    then ReadBlock
    else ReadBlockLineBuffer

readBlockLineBufferNotAllowed :: Bool -> Bool -> NESeq CommandP1 -> Bool
readBlockLineBufferNotAllowed isFileLog isFileLogMulti cmds =
  isMulti
    && isFileLog
    && not isFileLogMulti
  where
    isMulti = length cmds > 1

-- | Parses 'ReadStrategy'.
parseReadStrategy :: (MonadFail m) => m Text -> m ReadStrategy
parseReadStrategy = (>>= Utils.inversePrettyFail "read-strategy" readStrategyMeta)
{-# INLINEABLE parseReadStrategy #-}

-- | Available 'ReadStrategy' strings.
readStrategyMeta :: (IsString a) => Tuple2 Bool (List a)
readStrategyMeta = (False, ["block", "block-line-buffer"])
