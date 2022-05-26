-- | Runs specs.
module Unit.Specs (specs) where

import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Unit.Specs.ShellRun.Args qualified as Args
import Unit.Specs.ShellRun.Command qualified as Command
import Unit.Specs.ShellRun.Legend.Internal qualified as Legend.Internal
import Unit.Specs.ShellRun.Logging.Formatting qualified as Logging.Formatting
import Unit.Specs.ShellRun.Utils qualified as Utils

-- | Entry point for specs.
specs :: TestTree
specs =
  T.testGroup
    "HUnit tests"
    [ Args.specs,
      Command.specs,
      Legend.Internal.specs,
      Logging.Formatting.specs,
      Utils.specs
    ]
