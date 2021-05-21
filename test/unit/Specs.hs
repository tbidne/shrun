{-# LANGUAGE ImportQualifiedPost #-}

module Specs (specs) where

import Specs.ShellRun.Parsing.Commands qualified as ParseCommands
import Specs.ShellRun.Utils qualified as Utils
import Test.Tasty (TestTree)
import Test.Tasty qualified as T

specs :: IO TestTree
specs = T.testGroup "HSpec Specs" <$> allSpecs
  where
    allSpecs =
      (<>)
        <$> ParseCommands.specs
        <*> Utils.specs