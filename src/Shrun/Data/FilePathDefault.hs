{-# LANGUAGE TemplateHaskell #-}

-- | Provides the 'FilePathDefault' type.
--
-- @since 0.1
module Shrun.Data.FilePathDefault
  ( FilePathDefault (..),
    _FPDefault,
    _FPManual,
  )
where

import Data.Char qualified as Ch
import Shrun.Prelude

-- | FilePath option that includes a default possibility.
--
-- @since 0.1
data FilePathDefault
  = -- | @since 0.1
    FPDefault
  | -- | @since 0.1
    FPManual !FilePath
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.5
makePrisms ''FilePathDefault

-- | @since 0.5
instance DecodeTOML FilePathDefault where
  tomlDecoder = do
    f <- tomlDecoder
    case fmap Ch.toLower f of
      "default" -> pure FPDefault
      "" -> fail "Empty path given for --file-log"
      _ -> pure (FPManual f)
