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

import Shrun.Prelude

-- | FilePath option that includes none and default possibilities.
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
  tomlDecoder =
    tomlDecoder @Text <&> \case
      "default" -> FPDefault
      other -> FPManual $ unpack other
