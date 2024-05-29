{-# LANGUAGE UndecidableInstances #-}

module Shrun.Data.Text
  ( UnlinedText (..),

    -- * Creation
    fromText,
    fromTextReplace,
    unsafeUnlinedText,

    -- * Elimination
    toText,

    -- * Functions
    concat,
    intercalate,
    reallyUnsafeLiftUnlined,
  )
where

import Data.String (IsString (fromString))
import Data.Text qualified as T
import Shrun.Prelude

-- | Text after it has had all lines separated into different texts. We
-- introduce a newtype for clarity. The idea is that when we read arbitrary
-- text from a handle, this type serves as a witness that we have indeed split
-- the string along newlines. Then, in the normal case, we log each line
-- separately.
--
-- In exceptional cases (e.g. command names), we may choose to combine the
-- list back into a single text, according to some logic.
--
-- The constructor 'UnsafeUnlinedText' should only be used when we __know__
-- the text has no newlines and performance means a branch is undesirable
-- (e.g. streaming). If there is no performance impact, consider
-- 'unsafeUnlinedText' instead.
newtype UnlinedText = UnsafeUnlinedText {unUnlinedText :: Text}
  deriving stock (Eq, Show)
  deriving (Monoid, Semigroup) via Text

instance IsString UnlinedText where
  fromString = fromTextReplace . pack

instance
  ( k ~ A_Getter,
    a ~ Text,
    b ~ Text
  ) =>
  LabelOptic "unUnlinedText" k UnlinedText UnlinedText a b
  where
  labelOptic = to (\(UnsafeUnlinedText ts) -> ts)
  {-# INLINE labelOptic #-}

-- | Creates a list of 'UnlinedText'.
fromText :: Text -> List UnlinedText
fromText = fmap UnsafeUnlinedText . T.lines

-- | Creates a single 'UnlinedText' by replacing newlines with
-- whitespace.
fromTextReplace :: Text -> UnlinedText
fromTextReplace = UnsafeUnlinedText . T.replace "\n" " "

-- | Unsafe creation that throws error when the text contains newline(s).
unsafeUnlinedText :: (HasCallStack) => Text -> UnlinedText
unsafeUnlinedText txt =
  if '\n' `T.elem` txt
    then error $ "Unwanted newline in text: " <> unpack txt
    else UnsafeUnlinedText txt

-- NOTE: [Text Line Concatenation]
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

-- | Concats via 'toText'.
concat :: List UnlinedText -> UnlinedText
concat = UnsafeUnlinedText . toText

intercalate :: UnlinedText -> List UnlinedText -> UnlinedText
intercalate (UnsafeUnlinedText d) =
  UnsafeUnlinedText
    . T.intercalate d
    . fmap (view #unUnlinedText)

-- | Lifts a 'Text' function to 'UnlinedText'. Very unsafe in that we do not
-- check for errors i.e. if the parameter function introduces any newlines,
-- then this will silently succeed. This exists for performance.
reallyUnsafeLiftUnlined :: (Text -> Text) -> UnlinedText -> UnlinedText
reallyUnsafeLiftUnlined f (UnsafeUnlinedText t) = UnsafeUnlinedText (f t)
