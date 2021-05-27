{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun.Utils.Text
  ( NonEmptyText,
    unNonEmptyText,
    mkNonEmptyText,
    unsafeMkNonEmptyText,
    breakStripPoint,
  )
where

import Data.Text (Text)
import Data.Text qualified as T

newtype NonEmptyText = MkNonEmptyText {unNonEmptyText :: Text}
  deriving (Eq, Ord, Semigroup, Show)

mkNonEmptyText :: Text -> Maybe NonEmptyText
mkNonEmptyText "" = Nothing
mkNonEmptyText t = Just $ MkNonEmptyText t

unsafeMkNonEmptyText :: Text -> NonEmptyText
unsafeMkNonEmptyText "" = error "Passed empty to unsafeMkNonEmptyText!"
unsafeMkNonEmptyText t = MkNonEmptyText t

breakStripPoint :: NonEmptyText -> Text -> (Text, Text)
breakStripPoint (MkNonEmptyText point) txt = case T.breakOn point txt of
  (x, T.stripPrefix point -> Just y) -> (x, y)
  pair -> pair
