module Main (main) where

import Props qualified
import Specs qualified
import Test.Tasty qualified as T

main :: IO ()
main =
  Specs.specs >>= \specs ->
    T.defaultMain $ T.testGroup "Tests" [specs, Props.props]