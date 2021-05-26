{-# LANGUAGE ImportQualifiedPost #-}

module Props.ShellRun.Utils.Text (props) where

import Data.Text qualified as Txt
import Hedgehog ((===))
import Hedgehog qualified as H
import Props.Generators qualified as PGens
import ShellRun.Utils.Text qualified as TextUtils
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

props :: TestTree
props = T.testGroup "ShellRun.Utils.Text" [mkNonEmptyText, unsafeNonEmptyText]

mkNonEmptyText :: TestTree
mkNonEmptyText = TH.testProperty "mkNonEmptyText only succeeds on non-empty input" $
  H.property $ do
    t <- H.forAll PGens.genText
    let result = TextUtils.mkNonEmptyText t
    H.assert $ case result of
      Just _ -> Txt.length t > 0
      Nothing -> Txt.length t == 0

unsafeNonEmptyText :: TestTree
unsafeNonEmptyText = TH.testProperty "unsafeNonEmptyText succeeds on non-empty input" $
  H.property $ do
    t <- H.forAll PGens.getNonEmptyText
    let rawText = TextUtils.unNonEmptyText t
    -- Silly assertion since we can't try with empty text, but basically tests:
    --   1. We don't get a runtime error with non-empty text
    --   2. We don't receive empty text
    t === TextUtils.unsafeMkNonEmptyText rawText
    H.assert $ Txt.length rawText > 0