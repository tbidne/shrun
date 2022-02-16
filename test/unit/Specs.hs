-- | Runs specs.
module Specs (specs) where

import Specs.ShellRun.Command qualified as Command
import Specs.ShellRun.Data.TimeRep qualified as TimeRep
import Specs.ShellRun.Legend.Internal qualified as LegendI
import Specs.ShellRun.Parsing.Args qualified as ParseArgs
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
      Command.specs,
      TimeRep.specs,
      Utils.specs
    ]
