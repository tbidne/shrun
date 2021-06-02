{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Test.DocTest qualified as DocTest

main :: IO ()
main =
  DocTest.doctest
    [ "-isrc",
      "src/ShellRun/Math/NonNegative.hs",
      "src/ShellRun/Math/Positive.hs",
      "src/ShellRun/Parsing/Commands.hs",
      "src/ShellRun/Parsing/Legend/Internal.hs",
      "ShellRun.Utils.Internal",
      "ShellRun.Utils",
      "src/ShellRun/Utils/Text.hs"
    ]