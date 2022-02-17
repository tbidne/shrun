-- | Runs specs.
module Specs (specs) where

import Specs.ShellRun.Args qualified as Args
import Specs.ShellRun.Command qualified as Command
import Specs.ShellRun.Data.TimeRep qualified as Data.TimeRep
import Specs.ShellRun.Env qualified as Env
import Specs.ShellRun.Legend.Internal qualified as Legend.Internal
import Specs.ShellRun.Utils qualified as Utils
import Test.Tasty (TestTree)
import Test.Tasty qualified as T

-- | Entry point for specs.
specs :: TestTree
specs =
  T.testGroup
    "HUnit tests"
    [ Args.specs,
      Env.specs,
      Command.specs,
      Data.TimeRep.specs,
      Legend.Internal.specs,
      Utils.specs
    ]
