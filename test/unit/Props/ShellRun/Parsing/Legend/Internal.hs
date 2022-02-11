-- | Property tests for ShellRun.Parsing.Legend.Internal.
module Props.ShellRun.Parsing.Legend.Internal (props) where

import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.Text qualified as Txt
import Hedgehog (MonadGen, PropertyT, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import ShellRun.Data.Legend (LegendMap)
import ShellRun.Parsing.Legend.Internal qualified as Internal
import ShellRun.Prelude
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | Entry point for ShellRun.Parsing.Legend.Internal property tests.
props :: TestTree
props =
  T.testGroup
    "ShellRun.Parsing.Legend.Internal"
    [ successProps,
      failureProps
    ]

successProps :: TestTree
successProps = TH.testProperty "linesToMap success props" $
  -- NOTE: This set to 1,000 as we have had test errors before that got
  -- through (i.e. duplicate keys) because 100 was not enough, and
  -- it's still fast.
  H.withTests 1_000 $
    H.property $ do
      commands <- H.forAll genGoodLines
      let result = Internal.linesToMap commands
      case result of
        Left err -> do
          H.footnoteShow err
          H.failure
        Right legend -> do
          H.annotate "Unique keys in original list should match legend"
          verifySize commands legend

failureProps :: TestTree
failureProps = TH.testProperty "linesToMap failure props" $
  H.property $ do
    commands <- H.forAll genBadLines
    let result = Internal.linesToMap commands
    case result of
      Left _ -> H.success
      Right _ -> H.failure

verifySize :: List Text -> LegendMap -> PropertyT IO ()
verifySize commands legend = do
  H.annotateShow commands
  let noComments = filter (not . Txt.isPrefixOf "#") commands
      textKeys = fmap getKey noComments
      numUniqueKeys = length $ Set.fromList textKeys
      numLegendKeys = length $ Map.keys legend

  H.annotate $ "Commands: " <> show commands
  H.annotate $ "Legend: " <> show legend
  H.annotate $ "numUniqueKeys: " <> show numUniqueKeys
  H.annotate $ "numLegendKeys: " <> show numLegendKeys
  numLegendKeys === numUniqueKeys
  where
    getKey = fst . Txt.break (== '=')

genGoodLines :: MonadGen m => m (List Text)
genGoodLines = do
  keyVals <- Gen.list range genGoodLine
  comments <- Gen.list range genComment
  let (_, unique) = foldl' takeUnique (Set.empty, []) keyVals

  Gen.shuffle (unique <> comments)
  where
    range = Range.linearFrom 20 1 80
    takeUnique (foundKeys, newList) (MkGoodLine k v)
      | Set.member k foundKeys = (foundKeys, newList)
      | otherwise = (Set.insert k foundKeys, k <> "=" <> v : newList)

genComment :: MonadGen m => m Text
genComment = do
  c <- Gen.text range Gen.latin1
  pure $ "#" <> c
  where
    range = Range.linearFrom 20 1 80

data GoodLine = MkGoodLine
  { gkey :: Text,
    gvalue :: Text
  }
  deriving (Show)

genGoodLine :: MonadGen m => m GoodLine
genGoodLine = MkGoodLine <$> genKey <*> genVal

genKey :: MonadGen m => m Text
genKey = Gen.filterT noSpaceOrEquals $ Gen.text range Gen.latin1
  where
    range = Range.linearFrom 5 1 10
    noSpaceOrEquals = Txt.all (\c -> c /= ' ' && c /= '=')

genVal :: MonadGen m => m Text
genVal = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 10 1 30

genBadLines :: MonadGen m => m (List Text)
genBadLines = Gen.shuffle =<< (:) <$> genBadLine <*> genGoodLines

-- Since we have the format 'key=val' where val can also include '=', the only
-- way a line can be "bad" is if:
--   1. non-empty
--   2. not a comment (does not start with #)
--   3. has no '='
genBadLine :: MonadGen m => m Text
genBadLine = Gen.filterT noEquals $ Gen.text range Gen.latin1
  where
    range = Range.linearFrom 5 1 10
    noEquals t = Txt.head t /= '#' && Txt.all (/= '=') t
