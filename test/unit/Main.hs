-- | Runs unit tests.
module Main (main) where

import Props qualified
import ShellRun.Prelude
import Specs qualified
import Test.Tasty qualified as T

-- | Entry point for unit tests.
main :: IO ()
main =
  Specs.specs >>= \specs ->
    T.defaultMain $ T.testGroup "Tests" [specs, Props.props]
