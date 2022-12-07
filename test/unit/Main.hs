-- | Runs unit tests.
module Main (main) where

import Test.Tasty qualified as Tasty
import Unit.Prelude
import Unit.Props qualified
import Unit.Specs qualified

-- | Entry point for unit tests.
main :: IO ()
main =
  Tasty.defaultMain $
    testGroup
      "Unit tests"
      [ Unit.Specs.specs,
        Unit.Props.props
      ]
