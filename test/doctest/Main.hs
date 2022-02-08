module Main (main) where

import Data.String (String)
import ShellRun.Prelude
import Test.DocTest qualified as DocTest

main :: IO ()
main =
  DocTest.doctest $
    [ "-isrc",
      "src/ShellRun/Parsing/Commands.hs",
      "src/ShellRun/Parsing/Legend/Internal.hs",
      "src/ShellRun/Utils/Internal.hs",
      "src/ShellRun/Utils.hs",
      "src/ShellRun/Utils/Text.hs"
    ]
      <> exts

-- This is needed because DocTest does not read the cabal
-- file's default-extensions
exts :: [String]
exts =
  [ "-XDerivingVia",
    "-XFlexibleInstances",
    "-XImportQualifiedPost",
    "-XInstanceSigs",
    "-XLambdaCase",
    "-XMultiParamTypeClasses",
    "-XMultiWayIf",
    "-XNamedFieldPuns",
    "-XNoImplicitPrelude",
    "-XNumericUnderscores",
    "-XOverloadedStrings",
    "-XScopedTypeVariables",
    "-XStandaloneKindSignatures",
    "-XTupleSections",
    "-XTypeApplications",
    "-XTypeFamilies"
  ]
