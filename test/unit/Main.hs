-- | Runs unit tests.
module Main (main) where

import Unit.Prelude
import Unit.Shrun.Configuration.Args.Parsing qualified
import Unit.Shrun.Configuration.Data.WithDisabled qualified
import Unit.Shrun.Configuration.Legend qualified
import Unit.Shrun.IO.Types qualified
import Unit.Shrun.Logging.Formatting qualified
import Unit.Shrun.Utils qualified

-- | Entry point for unit tests.
main :: IO ()
main =
  defaultMain
    $ testGroup
      "Unit tests"
      [ Unit.Shrun.Configuration.Args.Parsing.tests,
        Unit.Shrun.Configuration.Data.WithDisabled.tests,
        Unit.Shrun.Configuration.Legend.tests,
        Unit.Shrun.IO.Types.tests,
        Unit.Shrun.Logging.Formatting.tests,
        Unit.Shrun.Utils.tests
      ]
