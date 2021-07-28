-- | Runs specs.
module Specs (specs) where

import ShellRun.Prelude
import Specs.ShellRun.Parsing.Args qualified as ParseArgs
import Specs.ShellRun.Parsing.Commands qualified as ParseCommands
import Specs.ShellRun.Parsing.Legend.Internal qualified as LegendI
import Specs.ShellRun.Utils qualified as Utils
import Specs.ShellRun.Utils.Text qualified as TextUtils
import Test.Tasty (TestTree)
import Test.Tasty qualified as T

-- | Entry point for specs.
specs :: IO TestTree
specs = T.testGroup "HSpec Specs" <$> allSpecs
  where
    allSpecs =
      join
        <$> sequenceA
          [ LegendI.specs,
            ParseArgs.specs,
            ParseCommands.specs,
            TextUtils.specs,
            Utils.specs
          ]
