{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Command' wrapper for commands.
--
-- @since 0.1
module ShellRun.Command
  ( Command (..),
    translateCommands,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.Hashable (Hashable)
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Text.Lazy qualified as LazyT
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as LTBuilder
import Refined qualified as R
import ShellRun.Data.NonEmptySeq (NonEmptySeq (..))
import ShellRun.Data.NonEmptySeq qualified as NESeq
import ShellRun.Legend (LegendErr (..), LegendMap)
import ShellRun.Prelude
import ShellRun.Utils qualified as U

-- $setup
-- >>> :set -XOverloadedLists

-- | Wrapper for shell commands.
--
-- @since 0.1
data Command = MkCommand
  { -- | The key name for the command, for display purposes.
    --
    -- @since 0.1
    getKey :: Maybe Text,
    -- | The shell command to run.
    --
    -- @since 0.1
    command :: Text
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      Hashable
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''Command

instance IsString Command where
  fromString = MkCommand Nothing . T.pack
  {-# INLINEABLE fromString #-}

-- | Returns a list of 'Text' commands, potentially transforming a
-- given string via the `LegendMap` @legend@.
--
-- Given a command string /s/, we first check if /s/ exists as a key in
-- @legend@. If it does not, we return /s/. If there is a key matching
-- /s/, i.e.,
--
-- @
-- legend = fromList [...,(s, v),...]
-- @
--
-- where \(v = v_1,,\ldots,,v_n\), then we recursively search on each
-- \(v_i\). We stop and return \(v_i\) when it does not exist as a key in the
-- map.
--
-- ==== __Examples__
-- >>> :{
--   let m = Map.fromList
--         [ ("cmd1", "one"),
--           ("cmd2", "two"),
--           ("all", "cmd1,,cmd2,,other")
--         ]
--       -- with -XOverloadedLists for Data.Seq literal
--       cmds = translateCommands m ("all" :|^ ["blah"])
--   in (fmap . fmap) (view #command) cmds
-- :}
-- Right ("one" :|^ fromList ["two","other","blah"])
--
-- Note: If -- when looking up a line -- we detect a cycle, then a 'CyclicKeyErr'
-- will be returned.
--
-- >>> :{
--   let m = Map.fromList
--         [ ("a", "b"),
--           ("b", "c"),
--           ("c", "a")
--         ]
--   in translateCommands m ("a" :|^ [])
-- :}
-- Left (CyclicKeyErr "a -> b -> c -> a")
--
-- @since 0.1
translateCommands :: LegendMap -> NonEmptySeq Text -> Either LegendErr (NonEmptySeq Command)
translateCommands mp (t :|^ ts) = sequenceA $ U.foldMap1 (lineToCommands mp) t ts
{-# INLINEABLE translateCommands #-}

lineToCommands :: LegendMap -> Text -> NonEmptySeq (Either LegendErr Command)
lineToCommands mp = go Nothing Set.empty (LTBuilder.fromText "")
  where
    -- The stringbuilder path is a textual representation of the key path
    -- we have traversed so far, e.g., a -> b -> c
    go prevKey foundKeys path line = case Map.lookup line mp of
      -- The line isn't a key, return it.
      Nothing -> NESeq.singleton (Right (MkCommand prevKey line))
      -- The line is a key, check for cycles and recursively
      -- call.
      Just val -> case maybeCyclicVal of
        Just cyclicVal ->
          let pathTxt = builderToPath path line cyclicVal
           in NESeq.singleton $ Left (CyclicKeyErr pathTxt)
        Nothing -> cmds >>= go (Just line) foundKeys' path'
        where
          -- WARN: Using a double comma right now as a delimiter. Hoping
          -- this isn't a thing that appears in real bash commands...
          cmds = U.splitOn $$(R.refineTH @R.NonEmpty @Text ",,") val
          foundKeys' = Set.insert line foundKeys

          -- Detect if we have an intersection between previously found
          -- keys and the values we just found. If so we have found a
          -- cyclic error.
          intersect = Set.intersection foundKeys (neToSet cmds)
          -- If there are cycles then this should be `Just cyclicVal`
          -- (this list should have at most one since we are detecting
          -- the first cycle)
          maybeCyclicVal = headMaybe $ Set.toList intersect
          path' = path <> LTBuilder.fromText line <> " -> "
          neToSet = Set.fromList . NESeq.toList
{-# INLINEABLE lineToCommands #-}

builderToPath :: Builder -> Text -> Text -> Text
builderToPath path l v =
  LazyT.toStrict $
    LTBuilder.toLazyText $
      path <> LTBuilder.fromText l <> " -> " <> LTBuilder.fromText v
{-# INLINEABLE builderToPath #-}
