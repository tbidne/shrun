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
    (putStrLn "*** Doc Tests disabled")
  where
    args = files <> exts

files :: [String]
files =
  [ "-isrc",
    "src/ShellRun/Configuration/Args.hs",
    "src/ShellRun/Configuration/Legend.hs",
    "src/ShellRun/Data/Command.hs",
    "src/ShellRun/Data/NonEmptySeq.hs",
    "src/ShellRun/Logging/Formatting.hs",
    "src/ShellRun/Logging/Queue.hs",
    "src/ShellRun/Utils.hs"
  ]

-- This is needed because DocTest does not read the cabal
-- file's default-extensions
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
