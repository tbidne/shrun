{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Timeout' type.
module Shrun.Data.Timeout
  ( Timeout (..),
    parseTimeout,
    parseTimeoutStr,
  )
where

import Data.Time.Relative qualified as RT
import Shrun.Prelude

-- | Represents a timeout, which is a non-negative integer.
newtype Timeout = MkTimeout
  { unTimeout :: Natural
  }
  deriving stock (Eq, Ord, Show)
  deriving (Num) via Natural

makeFieldLabelsNoPrefix ''Timeout

instance DecodeTOML Timeout where
  tomlDecoder =
    parseTimeout tomlDecoder tomlDecoder

-- NOTE: [CLI vs. Toml Types]
--
-- Normally we'd want CLI and Toml to share the exact same parsing, however,
-- we should take advantage of Toml's types when possible. For instance,
-- here we want to allow parsing a numeric nat or a "time string".
--
-- The CLI has to make do with strings everywhere, hence parseTimeout makes sense.

parseTimeout :: (Alternative f, MonadFail f) => f Natural -> f Text -> f Timeout
parseTimeout getNat getTxt =
  (MkTimeout <$> getNat) <|> (getTxt >>= parseTimeoutStr)

parseTimeoutStr :: (MonadFail f) => Text -> f Timeout
parseTimeoutStr txt = case RT.fromString str of
  Right n -> pure $ MkTimeout $ RT.toSeconds n
  Left err -> fail $ "Error reading time string: " <> err
  where
    str = unpack txt
