{-# LANGUAGE UndecidableInstances #-}

-- | Provides utils for stripping text whitespace.
module Shrun.Data.Text
  ( -- * Type
    StrippedText (MkStrippedText),

    -- * Creation
    stripText,
    stripLines,

    -- * Elimination
    unStrippedText,
    sepLines,

    -- * Misc
    stripLinesSep,
  )
where

import Data.Text qualified as T
import Shrun.Prelude

-- | Text that has been stripped of leading and trailing whitespace.
newtype StrippedText = UnsafeStrippedText Text
  deriving (Eq, Show) via Text

-- | Eliminates 'StrippedText'.
unStrippedText :: StrippedText -> Text
unStrippedText (UnsafeStrippedText t) = t

instance
  (k ~ A_Getter, a ~ Text, b ~ Text) =>
  LabelOptic "unStrippedText" k StrippedText StrippedText a b
  where
  labelOptic = to (\(UnsafeStrippedText t) -> t)
  {-# INLINE labelOptic #-}

pattern MkStrippedText :: Text -> StrippedText
pattern MkStrippedText t <- UnsafeStrippedText t
  where
    MkStrippedText t = stripText t

{-# COMPLETE MkStrippedText #-}

-- | Constructs 'StrippedText'
stripText :: Text -> StrippedText
stripText = UnsafeStrippedText . T.strip

-- | Splits text into lines and strips.
stripLines :: Text -> [StrippedText]
stripLines = fmap stripText . T.lines

-- | Concats lines with a single whitespace. Removes empties lines so that we
-- have exactly one single-space between all non-empty lines.
sepLines :: [StrippedText] -> Text
sepLines =
  T.intercalate " "
    . filter (not . T.null)
    . fmap unStrippedText

-- | Composition of stripLines and sepLines.
stripLinesSep :: Text -> Text
stripLinesSep = sepLines . stripLines
