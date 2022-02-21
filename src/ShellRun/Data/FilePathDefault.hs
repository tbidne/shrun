-- | Provides the 'FilePathDefault' type.
--
-- @since 0.1.0.0
module ShellRun.Data.FilePathDefault
  ( FilePathDefault (..),
  )
where

import ShellRun.Prelude

-- | FilePath option that includes none and default possibilities.
--
-- @since 0.1.0.0
data FilePathDefault
  = -- | @since 0.1.0.0
    FPNone
  | -- | @since 0.1.0.0
    FPDefault
  | -- | @since 0.1.0.0
    FPManual FilePath
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
instance Semigroup FilePathDefault where
  FPManual f <> _ = FPManual f
  _ <> FPManual f = FPManual f
  FPDefault <> _ = FPDefault
  FPNone <> r = r

-- | @since 0.1.0.0
instance Monoid FilePathDefault where
  mempty = FPNone
