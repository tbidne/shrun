-- | Property tests for ShellRun.Parsing.Commands.
module Props.ShellRun.Parsing.Commands
  ( props,
  )
where

import Data.Functor.Identity (Identity)
import Data.HashMap.Strict qualified as Map
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Hedgehog (GenBase, MonadGen, PropertyT)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import MaxRuns (MaxRuns (..))
import ShellRun.Data.Command qualified as Command
import ShellRun.Data.NonEmptySeq (NonEmptySeq)
import ShellRun.Data.NonEmptySeq qualified as NESeq
import ShellRun.Legend (LegendMap)
import ShellRun.Parsing.Commands qualified as ParseCommands
import ShellRun.Prelude
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | Entry point for ShellRun.Parsing.Commands property tests.
props :: TestTree
props = T.testGroup "ShellRun.Parsing.Commands" [translateProps]

translateProps :: TestTree
translateProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "translateCommands includes everything" $
    H.withTests limit $
      H.property $ do
        (legend, origCmds) <- H.forAll genLegendCommands
        let legendKeySet = Set.fromList $ Map.keys legend
            maybeFinalCmds = ParseCommands.translateCommands legend origCmds

        case maybeFinalCmds of
          Left err -> do
            H.footnote $ "Received a LegendErr: " <> show err
            H.failure
          Right finalCmds -> do
            let finalCmdsSet = Set.fromList $ NESeq.toList $ fmap Command.command finalCmds
                combinedKeySet = Set.union legendKeySet finalCmdsSet

            H.footnote $ "Final commands: " <> show finalCmdsSet
            H.footnote $ "Legend: " <> show legendKeySet
            noCommandsMissing combinedKeySet origCmds

-- Verify all of our original commands exist in the union:
--   LegendKeys \cup FinalCommands
noCommandsMissing :: HashSet Text -> NonEmptySeq Text -> PropertyT IO ()
noCommandsMissing allKeys = void . traverse failIfMissing
  where
    failIfMissing cmd
      | Set.member cmd allKeys = pure ()
      | otherwise = do
          H.footnote $ "Missing command: " <> show cmd
          H.failure

genLegendCommands :: (GenBase m ~ Identity, MonadGen m) => m (LegendMap, NonEmptySeq Text)
genLegendCommands = (,) <$> genLegend <*> genCommands

-- In order to avoid cycles -- e.g. a -> b -> a -- we disallow all recursive
-- references i.e. a val is not allowed to reference a key at all.
-- This is a bit of a sledgehammer approach, as we certainly allow
-- (non-cyclic) recursive references in our application. That said,
-- this stronger "no references" rule is significantly simpler to enforce
-- here.
genLegend :: (GenBase m ~ Identity, MonadGen m) => m LegendMap
genLegend = do
  keyVals <- Gen.list range genKeyVal
  let keySet = Set.fromList $ fmap fst keyVals
  let noCycles = foldl' (noKeyEqVal keySet) [] keyVals
  pure $ Map.fromList noCycles
  where
    range = Range.linearFrom 0 0 80
    noKeyEqVal ks acc p@(_, v)
      | cmdInSet ks v = acc
      | otherwise = p : acc
    -- Split RHS value into all cmds, reject if any reference a key
    cmdInSet s cmd =
      let cmds = T.splitOn ",," cmd
       in any (`Set.member` s) cmds

genKeyVal :: (GenBase m ~ Identity, MonadGen m) => m (Tuple2 Text Text)
genKeyVal = do
  k <- genKey
  -- Reject a=a key/val pairs, intended to avoid cycles. Technically
  -- unnecessary as genLegend rejects all recursive references,
  -- not just trivial a=a. But we might as well prevent ourselves from
  -- constructing these trivial bad values in the first place since:
  --  1. it's easy
  --  2. makes our test more robust (tests more values)
  --  3. The performance hit is negligible
  v <- Gen.filter (/= k) genVal
  pure (k, v)

genKey :: MonadGen m => m Text
genKey = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 1 1 10

genVal :: MonadGen m => m Text
genVal = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 1 1 50

genCommands :: MonadGen m => m (NonEmptySeq Text)
genCommands = NESeq.unsafeFromList <$> Gen.list range genCommand
  where
    range = Range.linearFrom 1 1 50

genCommand :: MonadGen m => m Text
genCommand = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 1 1 50
