-- | Runs specs.
module Unit.Specs (specs) where

import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Unit.Specs.ShellRun.Configuration.Args qualified as Config.Args
import Unit.Specs.ShellRun.Logging.Formatting qualified as Logging.Formatting
import Unit.Specs.ShellRun.Utils qualified as Utils

-- | Entry point for specs.
specs :: TestTree
specs =
  T.testGroup
    "HUnit tests"
    [ Config.Args.specs,
      Logging.Formatting.specs,
      Utils.specs
    ]
