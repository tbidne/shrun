{-# LANGUAGE UndecidableInstances #-}

module Shrun.Data.Text
  ( UnlinedText (..),
    fromText,
    toText,
  )
where

import Data.Text qualified as T
import Shrun.Prelude

-- | Text after it has had all lines separated into different texts. We
-- introduce a newtype for clarity. The idea is that when we read arbitrary
-- text from a handle, this type serves as a witness that we have indeed split
-- the string along newlines. Then, in the normal case, we log each line
-- separately.
--
-- In exceptional cases (e.g. command names), we may choose to combine the
-- list back into a single text, according to some logic. This is also handled
-- here via the pattern synonym.
newtype UnlinedText = UnsafeUnlinedText {unUnlinedText :: Text}
  deriving stock (Eq, Show)
  deriving (Monoid, Semigroup) via Text

instance
  ( k ~ A_Getter,
    a ~ Text,
    b ~ Text
  ) =>
  LabelOptic "unUnlinedText" k UnlinedText UnlinedText a b
  where
  labelOptic = to (\(UnsafeUnlinedText ts) -> ts)
  {-# INLINE labelOptic #-}

fromText :: Text -> List UnlinedText
fromText = fmap UnsafeUnlinedText . T.lines

-- NOTE: [Text Line Concatentation]
--
-- Normally, we log multiple newlines separately. However in at least one
-- case, we want a single log: final error message. Why? Because we want
-- exactly one [Success]/[Error] log, so if we have multiple stderr lines,
-- we need to combine these into a single log.
--
-- We have a choice on concatenation. We choose whitespace as the delimiter,
-- as newlines:
--
-- 1. Will get stripped during formatting (happens after this is called).
-- 2. Even if we special case to avoid 1, newlines probably won't look good
--    in the final output.
toText :: List UnlinedText -> Text
toText = T.intercalate " " . fmap (view #unUnlinedText)
