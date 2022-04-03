{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'FilePathDefault' type.
--
-- @since 0.1
module ShellRun.Data.FilePathDefault
  ( FilePathDefault (..),
  )
where

import ShellRun.Prelude

-- | FilePath option that includes none and default possibilities.
--
-- @since 0.1
data FilePathDefault
  = -- | @since 0.1
    FPNone
  | -- | @since 0.1
    FPDefault
  | -- | @since 0.1
    FPManual FilePath
  deriving
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Semigroup FilePathDefault where
  FPManual f <> _ = FPManual f
  _ <> FPManual f = FPManual f
  FPDefault <> _ = FPDefault
  FPNone <> r = r

-- | @since 0.1
instance Monoid FilePathDefault where
  mempty = FPNone

makePrismLabels ''FilePathDefault
