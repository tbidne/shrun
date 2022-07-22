-- | Runs integration tests.
module Main (main) where

import Integration.Defaults qualified as Defaults
import Integration.Examples qualified as Examples
import Integration.Failures qualified as Failures
import Integration.Prelude

-- | Entry point for integration tests.
main :: IO ()
main = do
  defaultMain (testGroup "Integration tests" tests)
    `finally` do
      deleteIfExists "log"
      deleteIfExists "test/integration/toml/log"
  where
    tests =
      [ Defaults.specs,
        Examples.specs,
        Failures.specs
      ]

