{-# LANGUAGE UndecidableInstances #-}

module Shrun.Data.CmdLogReadSize
  ( CmdLogReadSize (..),
  )
where

import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude

newtype CmdLogReadSize = MkCmdLogReadSize {unCmdLogReadSize :: Bytes B Natural}
  deriving stock (Eq, Show)

instance
  ( k ~ An_Iso,
    a ~ Bytes B Natural,
    b ~ Bytes B Natural
  ) =>
  LabelOptic
    "unCmdLogReadSize"
    k
    CmdLogReadSize
    CmdLogReadSize
    a
    b
  where
  labelOptic = iso (\(MkCmdLogReadSize x) -> x) MkCmdLogReadSize

instance Default CmdLogReadSize where
  def = MkCmdLogReadSize $ MkBytes 1024
