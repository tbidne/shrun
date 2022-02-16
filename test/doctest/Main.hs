module Main (main) where

import Data.String (String)
import ShellRun.Prelude
import System.Environment qualified as Env
import Test.DocTest qualified as DocTest

main :: IO ()
main = do
  shouldRun <- Env.lookupEnv "RUN_DOCTEST"
  case shouldRun of
    Just "true" -> DocTest.doctest args
    _ -> putStrLn "*** Doctests Disabled ***"
  where
    args = files <> exts

files :: List String
files =
  [ "-isrc",
    "src/ShellRun/Args.hs",
    "src/ShellRun/Command.hs",
    "src/ShellRun/Data/NonEmptySeq.hs",
    "src/ShellRun/Data/TimeRep.hs",
    "src/ShellRun/Legend/Internal.hs",
    "src/ShellRun/Utils.hs"
  ]

-- This is needed because DocTest does not read the cabal
-- file's default-extensions
exts :: List String
exts =
  [ "-XDerivingVia",
    "-XFlexibleContexts",
    "-XFlexibleInstances",
    "-XImportQualifiedPost",
    "-XInstanceSigs",
    "-XLambdaCase",
    "-XMultiParamTypeClasses",
    "-XMultiWayIf",
    "-XNamedFieldPuns",
    "-XNoImplicitPrelude",
    "-XNumericUnderscores",
    "-XOverloadedLists",
    "-XOverloadedStrings",
    "-XPatternSynonyms",
    "-XScopedTypeVariables",
    "-XStandaloneKindSignatures",
    "-XTupleSections",
    "-XTypeApplications",
    "-XTypeFamilies"
  ]
