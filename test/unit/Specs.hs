-- | Runs specs.
module Specs (specs) where

import Specs.ShellRun.Data.TimeRep qualified as TimeRep
import Specs.ShellRun.Parsing.Args qualified as ParseArgs
import Specs.ShellRun.Parsing.Commands qualified as ParseCommands
import Specs.ShellRun.Parsing.Legend.Internal qualified as LegendI
import Specs.ShellRun.Utils qualified as Utils
import Test.Tasty (TestTree)
import Test.Tasty qualified as T

-- | Entry point for specs.
specs :: TestTree
specs =
  T.testGroup
    "HUnit tests"
    [ LegendI.specs,
      ParseArgs.specs,
      ParseCommands.specs,
      TimeRep.specs,
      Utils.specs
    ]
