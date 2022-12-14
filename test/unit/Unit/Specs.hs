-- | Runs specs.
module Unit.Specs (specs) where

import Unit.Prelude
import Unit.Specs.Shrun.Configuration.Args qualified as Config.Args
import Unit.Specs.Shrun.Logging.Formatting qualified as Logging.Formatting
import Unit.Specs.Shrun.Utils qualified as Utils

-- | Entry point for specs.
specs :: TestTree
specs =
  testGroup
    "HUnit tests"
    [ Config.Args.specs,
      Logging.Formatting.specs,
      Utils.specs
    ]
