{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Internal module for parsing 'Text' lines into a 'LegendMap'.
--
-- @since 0.1.0.0
module ShellRun.Legend.Internal
  ( LegendErr (..),
    LegendMap,
    linesToMap,
  )
where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Refined (NonEmpty)
import Refined qualified as R
import ShellRun.Prelude
import ShellRun.Utils qualified as U

-- | Various errors that can occur while processing the legend.
--
-- @since 0.1.0.0
data LegendErr
  = -- | Errors relating to locating the legend file itself.
    --
    -- @since 0.1.0.0
    FileErr Text
  | -- | Errors relating to legend key=val format.
    --
    -- @since 0.1.0.0
    EntryErr Text
  | -- | Errors relating to cyclic keys.
    --
    -- @since 0.1.0.0
    CyclicKeyErr Text
  | -- | Errors relating to duplicate keys.
    --
    -- @since 0.1.0.0
    DuplicateKeyErr Text
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

-- | Alias for our legend map.
--
-- @since 0.1.0.0
type LegendMap = HashMap Text Text

-- | Attempts to parse the given ['Text'] into 'LegendMap'.
-- The text lines can either be comments (start with \'#\') or
-- key value pairs. The pairs have the form:
--
-- @
-- key=val
-- @
--
-- Parsing can fail if, for any non-comment line:
--
-- - Key is empty.
-- - Value is empty.
-- - There are duplicate keys.
--
-- ==== __Examples__
-- >>> linesToMap ["=val"]
-- Left (EntryErr "Key cannot be empty: =val")
--
-- >>> linesToMap ["key="]
-- Left (EntryErr "Value cannot be empty: key=")
--
-- >>> linesToMap ["key=value"]
-- Right (fromList [("key","value")])
--
-- >>> linesToMap ["key=value1","key=value2"]
-- Left (DuplicateKeyErr "key")
--
-- @since 0.1.0.0
linesToMap :: List Text -> Either LegendErr LegendMap
linesToMap = foldr f (Right Map.empty)
  where
    f "" mp = mp
    f (T.stripPrefix "#" -> Just _) mp = mp
    f line mp = join $ liftA2 insertPair (parseLine line) mp
    insertPair (key, cmd) mp =
      case Map.lookup key mp of
        Just _ -> Left $ DuplicateKeyErr key
        Nothing -> Right $ Map.insert key cmd mp

parseLine :: Text -> Either LegendErr (Tuple2 Text Text)
parseLine l =
  case U.breakStripPoint breakPoint l of
    ("", _) -> Left $ EntryErr $ "Key cannot be empty: " <> l
    (_, "") -> Left $ EntryErr $ "Value cannot be empty: " <> l
    (k, v) -> Right (k, v)
  where
    breakPoint = $$(R.refineTH @NonEmpty @Text "=")
