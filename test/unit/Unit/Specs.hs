-- | Runs specs.
module Unit.Specs (specs) where

import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Unit.Specs.Shrun.Configuration.Args qualified as Config.Args
import Unit.Specs.Shrun.Logging.Formatting qualified as Logging.Formatting
import Unit.Specs.Shrun.Utils qualified as Utils

-- | Entry point for specs.
specs :: TestTree
specs =
  T.testGroup
    "HUnit tests"
    [ Config.Args.specs,
      Logging.Formatting.specs,
      Utils.specs
    ]
