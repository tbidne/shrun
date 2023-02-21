module Main (main) where

import System.Environment.Guard (ExpectEnv (..), guardOrElse')
import Test.DocTest qualified as DocTest
import Prelude

main :: IO ()
main =
  guardOrElse'
    "RUN_DOCTEST"
    ExpectEnvSet
    (DocTest.doctest args)
    (putStrLn "*** Doc Tests disabled. Enable with RUN_DOCTEST=1")
  where
    args = files <> exts

files :: [String]
files =
  [ "-isrc",
    "src/Shrun/Configuration/Legend.hs",
    "src/Shrun/Data/Command.hs",
    "src/Shrun/Data/PollInterval.hs",
    "src/Shrun/Logging/Formatting.hs",
    "src/Shrun/Prelude.hs",
    "src/Shrun/Utils.hs"
  ]

exts :: [String]
exts =
  [ "-XNoImplicitPrelude",
    "-XNoStarIsType",
    "-XApplicativeDo",
    "-XBangPatterns",
    "-XDataKinds",
    "-XDeriveGeneric",
    "-XDerivingVia",
    "-XDuplicateRecordFields",
    "-XFlexibleContexts",
    "-XFlexibleInstances",
    "-XImportQualifiedPost",
    "-XInstanceSigs",
    "-XLambdaCase",
    "-XMultiParamTypeClasses",
    "-XMultiWayIf",
    "-XNamedFieldPuns",
    "-XNumericUnderscores",
    "-XOverloadedLabels",
    "-XOverloadedStrings",
    "-XPatternSynonyms",
    "-XScopedTypeVariables",
    "-XStandaloneKindSignatures",
    "-XTupleSections",
    "-XTypeApplications",
    "-XTypeFamilies"
  ]
