-- | Runs unit tests.
module Main (main) where

import Unit.Prelude
import Unit.Shrun.Command qualified
import Unit.Shrun.Configuration.Args.Parsing qualified
import Unit.Shrun.Configuration.Data.CommandLogging.ReadSize qualified
import Unit.Shrun.Configuration.Data.Graph
import Unit.Shrun.Configuration.Data.WithDisabled qualified
import Unit.Shrun.Configuration.Legend qualified
import Unit.Shrun.Configuration.Toml qualified
import Unit.Shrun.Data.Result qualified
import Unit.Shrun.IO.Handle qualified
import Unit.Shrun.Logging.Formatting qualified
import Unit.Shrun.Utils qualified

-- | Entry point for unit tests.
main :: IO ()
main =
  defaultMain
    $ testGroup
      "Unit tests"
      [ Unit.Shrun.Command.tests,
        Unit.Shrun.Configuration.Args.Parsing.tests,
        Unit.Shrun.Configuration.Data.CommandLogging.ReadSize.tests,
        Unit.Shrun.Configuration.Data.Graph.tests,
        Unit.Shrun.Configuration.Data.WithDisabled.tests,
        Unit.Shrun.Configuration.Legend.tests,
        Unit.Shrun.Configuration.Toml.tests,
        Unit.Shrun.Data.Result.tests,
        Unit.Shrun.IO.Handle.tests,
        Unit.Shrun.Logging.Formatting.tests,
        Unit.Shrun.Utils.tests
      ]
