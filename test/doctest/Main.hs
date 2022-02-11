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

files :: [String]
files =
  [ "-isrc",
    "src/ShellRun/Data/TimeRep.hs",
    "src/ShellRun/Parsing/Commands.hs",
    "src/ShellRun/Parsing/Legend/Internal.hs",
    "src/ShellRun/Utils.hs",
    "src/ShellRun/Utils/Text.hs"
  ]

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
