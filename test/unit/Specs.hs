{-# LANGUAGE ImportQualifiedPost #-}

module Specs (specs) where

import Specs.ShellRun.Parsing.Commands qualified as ParseCommands
import Specs.ShellRun.Parsing.Legend.Internal qualified as LegendI
import Specs.ShellRun.Utils qualified as Utils
import Specs.ShellRun.Utils.Text qualified as TextUtils
import Test.Tasty (TestTree)
import Test.Tasty qualified as T

specs :: IO TestTree
specs = T.testGroup "HSpec Specs" <$> allSpecs
  where
    allSpecs =
      (\a b c d -> a <> b <> c <> d)
        <$> LegendI.specs
        <*> ParseCommands.specs
        <*> TextUtils.specs
        <*> Utils.specs