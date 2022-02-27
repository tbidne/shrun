-- | Runs specs.
module Unit.Specs (specs) where

import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Unit.Specs.ShellRun.Args qualified as Args
import Unit.Specs.ShellRun.Command qualified as Command
import Unit.Specs.ShellRun.Data.TimeRep qualified as Data.TimeRep
import Unit.Specs.ShellRun.Legend.Internal qualified as Legend.Internal
import Unit.Specs.ShellRun.Utils qualified as Utils

-- | Entry point for specs.
specs :: TestTree
specs =
  T.testGroup
    "HUnit tests"
    [ Args.specs,
      Command.specs,
      Data.TimeRep.specs,
      Legend.Internal.specs,
      Utils.specs
    ]
