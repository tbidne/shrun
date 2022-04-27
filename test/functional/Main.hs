-- | Runs functional tests.
module Main (main) where

import Functional.Prelude
import Functional.Success qualified as Success
import Functional.SuccessCommandLogging qualified as SuccessCommandLogging
import Functional.SuccessFileLogging qualified as SuccessFileLogging
import Functional.SuccessNoLogging qualified as SuccessNoLogging
import Functional.SuccessShowKey qualified as SuccessShowKey
import Functional.TestArgs (TestArgs (..))
import Functional.Timeout qualified as Timeout
import Functional.Truncation qualified as Truncation
import System.Directory qualified as Dir
import System.FilePath ((</>))
import Test.Tasty qualified as Tasty

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
      SuccessNoLogging.spec,
      Timeout.spec,
      Truncation.spec
    ]

setup :: IO TestArgs
setup = do
  cwd <- (</> "test/functional") <$> Dir.getCurrentDirectory
  let td = cwd </> "tmp"
      lp = cwd </> "shell-run.legend"

  Dir.createDirectoryIfMissing False td
  pure $
    MkTestArgs
      { tmpDir = td,
        legendPath = lp
      }

teardown :: TestArgs -> IO ()
teardown = Dir.removeDirectory . view #tmpDir
