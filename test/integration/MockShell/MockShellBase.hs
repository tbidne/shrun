module MockShell.MockShellBase (MockShellBase (..)) where

import Data.Text (Text)

data MockShellBase a = MkMockShellBase a [Text]

instance Functor MockShellBase where
  fmap f (MkMockShellBase x lg) = MkMockShellBase (f x) lg

instance Applicative MockShellBase where
  pure x = MkMockShellBase x []
  MkMockShellBase f lhs <*> MkMockShellBase x rhs = MkMockShellBase (f x) (lhs <> rhs)

instance Monad MockShellBase where
  MkMockShellBase x lhs >>= f = MkMockShellBase y (lhs <> rhs)
    where
      MkMockShellBase y rhs = f x

instance Show a => Show (MockShellBase a) where
  show (MkMockShellBase x lg) = "MockShellBase " <> show x <> " " <> show lg

instance Semigroup a => Semigroup (MockShellBase a) where
  MkMockShellBase x lhs <> MkMockShellBase y rhs = MkMockShellBase (x <> y) (lhs <> rhs)

instance Monoid a => Monoid (MockShellBase a) where
  mempty = MkMockShellBase mempty mempty