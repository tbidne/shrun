{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.CommandLogging.ReadStrategy
  ( ReadStrategy (..),
    parseReadStrategy,
    readStrategyMeta,
    defaultReadStrategy,
    readBlockLineBufferNotAllowed,
  )
where

import Shrun.Configuration.Data.Graph (CommandGraph)
import Shrun.Configuration.Data.Graph qualified as Graph
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

defaultReadStrategy :: Bool -> Bool -> CommandGraph -> ReadStrategy
defaultReadStrategy fileLogOn fileLogMultiOn cmdGraph =
  if readBlockLineBufferNotAllowed fileLogOn fileLogMultiOn cmdGraph
    then ReadBlock
    else ReadBlockLineBuffer

-- Block line buffer not allowed when /all/ of the following are true:
--
-- - file logging: on
-- - file multi log: off
-- - commands: concurrent
readBlockLineBufferNotAllowed :: Bool -> Bool -> CommandGraph -> Bool
readBlockLineBufferNotAllowed fileLogOn fileLogMultiOn cmdGraph =
  isConcurrent
    && fileLogOn
    && not fileLogMultiOn
  where
    isConcurrent = not (Graph.isSequential cmdGraph)

-- | Parses 'ReadStrategy'.
parseReadStrategy :: (MonadFail m) => m Text -> m ReadStrategy
parseReadStrategy = (>>= Utils.inversePrettyFail "read-strategy" readStrategyMeta)
{-# INLINEABLE parseReadStrategy #-}

-- | Available 'ReadStrategy' strings.
readStrategyMeta :: (IsString a) => Tuple2 Bool (List a)
readStrategyMeta = (False, ["block", "block-line-buffer"])
