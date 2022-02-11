-- | Property tests for ShellRun.Parsing.Commands.
module Props.ShellRun.Parsing.Commands
  ( props,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Hedgehog (MonadGen, PropertyT)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import ShellRun.Data.Command qualified as Command
import ShellRun.Data.Legend (LegendMap)
import ShellRun.Parsing.Commands qualified as ParseCommands
import ShellRun.Prelude
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | Entry point for ShellRun.Parsing.Commands property tests.
props :: TestTree
props = T.testGroup "ShellRun.Parsing.Commands" [translateProps]

translateProps :: TestTree
translateProps = TH.testProperty "translateCommands includes everything" $
  H.property $ do
    (legend, origCmds) <- H.forAll genLegendCommands
    let legendKeySet = Set.fromList $ Map.keys legend
        maybeFinalCmds = ParseCommands.translateCommands legend origCmds

    case maybeFinalCmds of
      Left err -> do
        H.footnote $ "Received a LegendErr: " <> show err
        H.failure
      Right finalCmds -> do
        let finalCmdsSet = Set.fromList $ fmap Command.command finalCmds
            combinedKeySet = Set.union legendKeySet finalCmdsSet

        H.footnote $ "Final commands: " <> show finalCmdsSet
        H.footnote $ "Legend: " <> show legendKeySet
        noCommandsMissing combinedKeySet origCmds

-- Verify all of our original commands exist in the union:
--   LegendKeys \cup FinalCommands
noCommandsMissing :: HashSet Text -> List Text -> PropertyT IO ()
noCommandsMissing allKeys = void . traverse failIfMissing
  where
    failIfMissing cmd
      | Set.member cmd allKeys = pure ()
      | otherwise = do
          H.footnote $ "Missing command: " <> show cmd
          H.failure

genLegendCommands :: MonadGen m => m (LegendMap, List Text)
genLegendCommands = (,) <$> genLegend <*> genCommands

-- WARN: This can technically generate a map that has cycles in it,
-- e.g., a -> b -> c -> a, which would cause an infinite loop if
-- we also happen to generate a command in that cycle. The odds of this
-- happening have to be really low, so not worrying about this for now...
genLegend :: MonadGen m => m LegendMap
genLegend = Map.fromList <$> Gen.list range genKeyVal
  where
    range = Range.linearFrom 0 0 80

genKeyVal :: MonadGen m => m (Tuple2 Text Text)
genKeyVal = (,) <$> genKey <*> genVal

genKey :: MonadGen m => m Text
genKey = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 1 1 10

genVal :: MonadGen m => m Text
genVal = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 1 1 50

genCommands :: MonadGen m => m (List Text)
genCommands = Gen.list range genCommand
  where
    range = Range.linearFrom 1 1 50

genCommand :: MonadGen m => m Text
genCommand = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 1 1 50
