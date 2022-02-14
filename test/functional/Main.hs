-- | Runs functional tests.
module Main (main) where

import ShellRun.Prelude
import Success qualified
import SuccessCommandLogging qualified
import SuccessFileLogging qualified
import SuccessShowKey qualified
import System.Directory qualified as Dir
import System.FilePath ((</>))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import TestArgs
import Timeout qualified

-- | Entry point for functional tests.
main :: IO ()
main =
  Tasty.defaultMain $ Tasty.withResource setup teardown specs

specs :: IO TestArgs -> TestTree
specs args = do
  Tasty.testGroup
    "Functional Tests"
    [ Success.spec args,
      SuccessShowKey.spec args,
      SuccessCommandLogging.spec args,
      SuccessFileLogging.spec args,
      Timeout.spec
    ]

setup :: IO TestArgs
setup = do
  cwd <- (</> "test/functional") <$> Dir.getCurrentDirectory
  let td = cwd </> "tmp"
      lp = cwd </> "legend.txt"

  Dir.createDirectoryIfMissing False td
  pure $
    MkTestArgs
      { tTmpDir = td,
        tLegendPath = lp
      }

teardown :: TestArgs -> IO ()
teardown = Dir.removeDirectory . tTmpDir
