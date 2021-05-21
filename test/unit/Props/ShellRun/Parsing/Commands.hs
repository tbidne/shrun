{-# LANGUAGE ImportQualifiedPost #-}

module Props.ShellRun.Parsing.Commands
  ( props,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Hedgehog (MonadGen, PropertyT)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import ShellRun.Parsing.Commands qualified as ParseCommands
import ShellRun.Types.Command qualified as Command
import ShellRun.Types.Legend (LegendMap)
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

props :: TestTree
props = T.testGroup "ShellRun.Parsing.Commands" [translateProps]

translateProps :: TestTree
translateProps = TH.testProperty "translateCommands includes everything" $
  H.property $ do
    (legend, origCmds) <- H.forAll genLegendCommands
    let legendKeySet = Set.fromList $ Map.keys legend
        finalCmds = ParseCommands.translateCommands legend origCmds
        finalCmdsSet = Set.fromList $ fmap Command.getCommand finalCmds
        combinedKeySet = Set.union legendKeySet finalCmdsSet

    H.footnote $ "Final commands: " <> show finalCmdsSet
    H.footnote $ "Legend: " <> show legendKeySet
    noCommandsMissing combinedKeySet origCmds

-- Verify all of our original commands exist in the union:
--   LegendKeys \cup FinalCommands
noCommandsMissing :: Set Text -> [Text] -> PropertyT IO ()
noCommandsMissing _ [] = pure ()
noCommandsMissing allKeys (cmd : origCmds)
  | Set.member cmd allKeys = noCommandsMissing allKeys origCmds
  | otherwise = do
    H.footnote $ "Missing command: " <> show cmd
    H.failure

genLegendCommands :: MonadGen m => m (LegendMap, [Text])
genLegendCommands = (,) <$> genLegend <*> genCommands

genLegend :: MonadGen m => m LegendMap
genLegend = Map.fromList <$> Gen.list range genKeyVal
  where
    range = Range.linearFrom 20 1 80

genKeyVal :: MonadGen m => m (Text, Text)
genKeyVal = (,) <$> genKey <*> genVal

genKey :: MonadGen m => m Text
genKey = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 5 1 10

genVal :: MonadGen m => m Text
genVal = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 10 1 50

genCommands :: MonadGen m => m [Text]
genCommands = Gen.list range genCommand
  where
    range = Range.linearFrom 10 1 50

genCommand :: MonadGen m => m Text
genCommand = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 10 1 50