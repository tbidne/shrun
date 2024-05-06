{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.CmdLogging.ReadSize
  ( ReadSize (..),
  )
where

import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude

-- | Read size for command logs.
newtype ReadSize = MkReadSize {unReadSize :: Bytes B Natural}
  deriving stock (Eq, Show)

instance
  ( k ~ An_Iso,
    a ~ Bytes B Natural,
    b ~ Bytes B Natural
  ) =>
  LabelOptic
    "unReadSize"
    k
    ReadSize
    ReadSize
    a
    b
  where
  labelOptic = iso (\(MkReadSize x) -> x) MkReadSize

instance Default ReadSize where
  def = MkReadSize $ MkBytes 1024
