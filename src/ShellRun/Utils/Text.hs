{-# LANGUAGE ViewPatterns #-}

-- | Provides 'Text' utils.
module ShellRun.Utils.Text
  ( NonEmptyText,
    unNonEmptyText,
    mkNonEmptyText,
    unsafeMkNonEmptyText,
    breakStripPoint,
  )
where

import Data.Text qualified as T
import ShellRun.Prelude

-- | Newtype wrapper for non-empty 'Text'.
newtype NonEmptyText = MkNonEmptyText
  { -- | Unwraps the 'NonEmptyText'
    unNonEmptyText :: Text
  }
  deriving (Eq, Ord, Semigroup, Show) via Text

-- | Smart constructor for 'NonEmptyText'.
mkNonEmptyText :: Text -> Maybe NonEmptyText
mkNonEmptyText "" = Nothing
mkNonEmptyText t = Just $ MkNonEmptyText t

-- | Unsafe constructor for 'NonEmptyText', intended to be used with
-- know constants, e.g., @unsafeMkNonEmptyText "hi"@. Exercise restraint!
unsafeMkNonEmptyText :: Text -> NonEmptyText
unsafeMkNonEmptyText "" = error "Passed empty to unsafeMkNonEmptyText!"
unsafeMkNonEmptyText t = MkNonEmptyText t

-- | Wrapper for 'Text'\'s 'T.breakOn' that differs in two ways:
--
-- 1. Total, since we restrict the @needle@ to 'NonEmptyText'
-- ('Text' throws a pure exception here).
-- 2. If the @needle@ is found within the @haystack@, we do not include it
-- in the second part of the pair.
--
-- For instance:
--
-- >>> -- Data.Text
-- >>> T.breakOn "=" "HEY=LISTEN"
-- ("HEY","=LISTEN")
--
-- >>> -- ShellRun.Utils.Text
-- >>> breakStripPoint (unsafeMkNonEmptyText "=") "HEY=LISTEN"
-- ("HEY","LISTEN")
--
-- Other examples:
--
-- >>> breakStripPoint (unsafeMkNonEmptyText "=") "HEYLISTEN"
-- ("HEYLISTEN","")
--
-- >>> breakStripPoint (unsafeMkNonEmptyText "=") "=HEYLISTEN"
-- ("","HEYLISTEN")
--
-- >>> breakStripPoint (unsafeMkNonEmptyText "=") "HEYLISTEN="
-- ("HEYLISTEN","")
--
-- >>> breakStripPoint (unsafeMkNonEmptyText "=") "HEY==LISTEN"
-- ("HEY","=LISTEN")
breakStripPoint :: NonEmptyText -> Text -> Tuple2 Text Text
breakStripPoint (MkNonEmptyText point) txt = case T.breakOn point txt of
  (x, T.stripPrefix point -> Just y) -> (x, y)
  pair -> pair
